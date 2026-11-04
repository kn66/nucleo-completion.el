"""Exercise release publication against a local Git remote and a fake GitHub CLI."""

import hashlib
import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest


SCRIPT = Path(__file__).resolve().parents[1] / '.github/scripts/publish-modules.sh'
BINARIES = (
    'nucleo-completion-module-x86_64-unknown-linux-gnu.so',
    'nucleo-completion-module-x86_64-apple-darwin.dylib',
    'nucleo-completion-module-aarch64-apple-darwin.dylib',
    'nucleo-completion-module-x86_64-pc-windows-msvc.dll',
)
FAKE_GH = r'''#!/usr/bin/env python3
import json, os, pathlib, subprocess, sys
args = sys.argv[1:]
state_file = pathlib.Path(os.environ['FAKE_GH_STATE'])
state = json.loads(state_file.read_text())
with open(os.environ['FAKE_GH_LOG'], 'a') as log:
    log.write(json.dumps(args) + '\n')
if args[0] == 'api':
    error = os.environ.get('API_ERROR')
    if error or state['release'] == 'missing':
        print('gh: request failed (HTTP ' + (error or '404') + ')', file=sys.stderr)
        sys.exit(1)
    print('true' if state['release'] == 'draft' else 'false')
elif args[:2] == ['release', 'create']:
    assert '--draft' in args and '--verify-tag' in args
    state['release'] = 'draft'
elif args[:2] == ['release', 'upload']:
    assert state['release'] == 'draft'
    assert len([a for a in args if a.startswith('artifacts/')]) == 12
    if os.environ.get('FAIL_UPLOAD') == '1':
        sys.exit(1)
    if os.environ.get('MOVE_TAG') == '1':
        subprocess.run(['git', '--git-dir', os.environ['FAKE_REMOTE'],
                        'update-ref', 'refs/tags/v0.1.16', os.environ['OLD_SHA']], check=True)
elif args[:2] == ['release', 'edit']:
    assert '--draft=false' in args
    state['release'] = 'published'
else:
    raise AssertionError(args)
state_file.write_text(json.dumps(state))
'''


class PublishModulesTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix='nucleo-publication-')
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        self.work = self.root / 'work'
        self.remote = self.root / 'remote.git'
        self.work.mkdir()
        self.git('init', '--bare', str(self.remote))
        self.git('init')
        self.git('config', 'user.name', 'Release Fixture')
        self.git('config', 'user.email', 'fixture@example.invalid')
        self.git('remote', 'add', 'origin', str(self.remote))
        (self.work / 'source').write_text('first')
        self.git('add', 'source')
        self.git('commit', '-m', 'first')
        self.old_sha = self.git('rev-parse', 'HEAD')
        (self.work / 'source').write_text('second')
        self.git('commit', '-am', 'second')
        self.sha = self.git('rev-parse', 'HEAD')
        (self.work / 'artifacts').mkdir()
        for binary in BINARIES:
            asset = self.work / 'artifacts' / binary
            asset.write_bytes(binary.encode())
            digest = hashlib.sha256(asset.read_bytes()).hexdigest()
            asset.with_name(binary + '.sha256').write_text(f'{digest}  dist/{binary}\n')
            asset.with_name(binary + '.licenses.txt').write_text('Fixture license\n')
        self.state_file = self.root / 'state.json'
        self.set_release('missing')
        self.log_file = self.root / 'gh.log'
        commands = self.root / 'commands'
        commands.mkdir()
        gh = commands / 'gh'
        gh.write_text(FAKE_GH)
        gh.chmod(0o755)
        self.environment = dict(
            os.environ, PATH=str(commands) + os.pathsep + os.environ['PATH'],
            RELEASE_TAG='v0.1.16', GITHUB_SHA=self.sha, GITHUB_EVENT_NAME='push',
            GH_REPO='fixture/repository', FAKE_GH_STATE=str(self.state_file),
            FAKE_GH_LOG=str(self.log_file), FAKE_REMOTE=str(self.remote), OLD_SHA=self.old_sha,
        )

    def git(self, *args):
        return subprocess.check_output(
            ['git', *args], cwd=self.work, stderr=subprocess.DEVNULL, text=True
        ).strip()

    def set_release(self, state):
        self.state_file.write_text(json.dumps({'release': state}))

    def remote_tag(self):
        return self.git('ls-remote', '--tags', 'origin', 'refs/tags/v0.1.16')

    def push_tag(self, sha=None, annotated=False):
        args = ['tag'] + (['-a', '-m', 'Fixture'] if annotated else [])
        self.git(*args, 'v0.1.16', sha or self.sha)
        self.git('push', 'origin', 'refs/tags/v0.1.16')

    def publish(self, **settings):
        return subprocess.run(
            ['bash', str(SCRIPT)], cwd=self.work, env=dict(self.environment, **settings),
            capture_output=True, text=True,
        )

    def calls(self):
        if not self.log_file.exists():
            return []
        return [json.loads(line) for line in self.log_file.read_text().splitlines()]

    def test_new_release_uploads_before_publication(self):
        result = self.publish()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertTrue(self.remote_tag().startswith(self.sha))
        self.assertEqual([c[:2] for c in self.calls()[1:]],
                         [['release', 'create'], ['release', 'upload'], ['release', 'edit']])
        self.assertEqual(json.loads(self.state_file.read_text())['release'], 'published')

    def test_published_release_is_unchanged(self):
        self.push_tag()
        self.set_release('published')
        result = self.publish()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(len(self.calls()), 1)

    def test_different_tag_is_skipped_on_push_and_rejected_manually(self):
        self.push_tag(self.old_sha)
        self.assertEqual(self.publish().returncode, 0)
        self.assertNotEqual(self.publish(GITHUB_EVENT_NAME='workflow_dispatch').returncode, 0)
        self.assertEqual(self.calls(), [])
        self.assertTrue(self.remote_tag().startswith(self.old_sha))

    def test_annotated_tag_draft_can_be_resumed(self):
        self.push_tag(annotated=True)
        self.set_release('draft')
        result = self.publish()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertNotIn(['release', 'create'], [c[:2] for c in self.calls()])
        self.assertEqual(json.loads(self.state_file.read_text())['release'], 'published')

    def test_failed_upload_stays_draft_and_retry_succeeds(self):
        self.assertNotEqual(self.publish(FAIL_UPLOAD='1').returncode, 0)
        self.assertEqual(json.loads(self.state_file.read_text())['release'], 'draft')
        self.assertNotIn(['release', 'edit'], [c[:2] for c in self.calls()])
        result = self.publish()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(json.loads(self.state_file.read_text())['release'], 'published')

    def test_github_inspection_failure_does_not_create_a_tag(self):
        for code in ('403', '503'):
            with self.subTest(code=code):
                self.assertNotEqual(self.publish(API_ERROR=code).returncode, 0)
                self.assertEqual(self.remote_tag(), '')
                self.assertTrue(all(c[0] == 'api' for c in self.calls()))

    def test_git_inspection_failure_stops_before_github(self):
        self.git('remote', 'set-url', 'origin', str(self.root / 'missing.git'))
        self.assertNotEqual(self.publish().returncode, 0)
        self.assertEqual(self.calls(), [])

    def test_missing_artifact_prevents_publication(self):
        (self.work / 'artifacts' / BINARIES[0]).unlink()
        self.assertNotEqual(self.publish().returncode, 0)
        self.assertEqual(self.calls(), [])
        self.assertEqual(self.remote_tag(), '')

    def test_extra_artifact_prevents_publication(self):
        (self.work / 'artifacts' / 'unexpected').write_text('unexpected')
        self.assertNotEqual(self.publish().returncode, 0)
        self.assertEqual(self.calls(), [])

    def test_checksum_mismatch_prevents_publication(self):
        (self.work / 'artifacts' / BINARIES[0]).write_bytes(b'changed')
        self.assertNotEqual(self.publish().returncode, 0)
        self.assertEqual(self.calls(), [])
        self.assertEqual(self.remote_tag(), '')

    def test_tag_change_during_upload_does_not_publish(self):
        result = self.publish(MOVE_TAG='1')
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(json.loads(self.state_file.read_text())['release'], 'draft')
        self.assertNotIn(['release', 'edit'], [c[:2] for c in self.calls()])


if __name__ == '__main__':
    unittest.main()
