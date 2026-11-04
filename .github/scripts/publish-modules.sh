#!/usr/bin/env bash
set -euo pipefail

: "${RELEASE_TAG:?RELEASE_TAG is required}"
: "${GITHUB_SHA:?GITHUB_SHA is required}"
: "${GITHUB_EVENT_NAME:?GITHUB_EVENT_NAME is required}"
: "${GH_REPO:?GH_REPO is required}"

git check-ref-format "refs/tags/$RELEASE_TAG"

# Check the complete set before creating a tag or touching a release.  In
# particular, do not accidentally publish files from another workflow run.
assets=()
for binary in \
  nucleo-completion-module-x86_64-unknown-linux-gnu.so \
  nucleo-completion-module-x86_64-apple-darwin.dylib \
  nucleo-completion-module-aarch64-apple-darwin.dylib \
  nucleo-completion-module-x86_64-pc-windows-msvc.dll; do
  for suffix in '' .sha256 .licenses.txt; do
    asset="artifacts/${binary}${suffix}"
    if [[ ! -f "$asset" || -L "$asset" ]]; then
      echo "Missing release asset: $asset" >&2
      exit 1
    fi
    assets+=("$asset")
  done
  # Check the digest without following the path recorded by the build runner.
  read -r expected_digest _ < "artifacts/${binary}.sha256"
  actual_digest="$(sha256sum "artifacts/$binary")"
  if [[ ! "$expected_digest" =~ ^[0-9a-f]{64}$ || "${actual_digest%% *}" != "$expected_digest" ]]; then
    echo "Checksum mismatch for $binary" >&2
    exit 1
  fi
done
mapfile -d '' actual_assets < <(find artifacts -mindepth 1 -print0)
if [[ ${#actual_assets[@]} -ne ${#assets[@]} ]]; then
  echo "Unexpected files or directories in artifacts; expected exactly ${#assets[@]} files" >&2
  exit 1
fi

# ls-remote gives the dereferenced object for annotated tags as a second line.
# Preserve command failures so an authentication or network error cannot look like
# an absent tag.
remote_tag_commit() {
  local refs direct peeled
  refs="$(git ls-remote --tags origin "refs/tags/$RELEASE_TAG" "refs/tags/$RELEASE_TAG^{}")" || return $?
  direct="$(awk -v ref="refs/tags/$RELEASE_TAG" '$2 == ref { print $1 }' <<< "$refs")"
  peeled="$(awk -v ref="refs/tags/$RELEASE_TAG^{}" '$2 == ref { print $1 }' <<< "$refs")"
  printf '%s' "${peeled:-$direct}"
}

check_tag_commit() {
  local commit="$1"
  if [[ -n "$commit" && "$commit" != "$GITHUB_SHA" ]]; then
    echo "Tag $RELEASE_TAG points to $commit, not built commit $GITHUB_SHA; bump the version to publish new binaries." >&2
    if [[ "$GITHUB_EVENT_NAME" == push ]]; then
      exit 0
    fi
    exit 1
  fi
}

tag_commit="$(remote_tag_commit)"
check_tag_commit "$tag_commit"

# A 404 is the only response that means the release does not exist.  All
# other errors must stop publication, including token and network failures.
release_error="$(mktemp)"
trap 'rm -f "$release_error"' EXIT
release_state='missing'
if release_draft="$(gh api "repos/$GH_REPO/releases/tags/$RELEASE_TAG" --jq .draft 2>"$release_error")"; then
  case "$release_draft" in
    true) release_state='draft' ;;
    false) release_state='published' ;;
    *) echo "Could not determine release draft status for $RELEASE_TAG" >&2; exit 1 ;;
  esac
elif ! grep -Eq 'HTTP(/[^ ]+)? 404' "$release_error"; then
  cat "$release_error" >&2
  echo "Could not inspect release $RELEASE_TAG" >&2
  exit 1
fi

if [[ "$release_state" == published ]]; then
  echo "Release $RELEASE_TAG is already published; leaving it unchanged."
  exit 0
fi
if [[ "$release_state" == draft && -z "$tag_commit" ]]; then
  echo "Draft release $RELEASE_TAG has no remote tag; refusing to resume." >&2
  exit 1
fi

if [[ -z "$tag_commit" ]]; then
  # A non-force push creates the ref only if it is still absent.  Reinspect it
  # after failure to handle another publisher racing us without moving a tag.
  if ! git push origin "$GITHUB_SHA:refs/tags/$RELEASE_TAG"; then
    tag_commit="$(remote_tag_commit)"
    check_tag_commit "$tag_commit"
    if [[ -z "$tag_commit" ]]; then
      echo "Could not create tag $RELEASE_TAG" >&2
      exit 1
    fi
  fi
fi

if [[ "$release_state" == missing ]]; then
  gh release create "$RELEASE_TAG" --verify-tag --draft \
    --title "$RELEASE_TAG" \
    --notes "Prebuilt nucleo-completion dynamic modules for $RELEASE_TAG."
fi
# Replacing an asset is safe while the release is a draft and permits a failed
# upload to resume. The public release is only exposed after every upload works.
gh release upload "$RELEASE_TAG" "${assets[@]}" --clobber
tag_commit="$(remote_tag_commit)"
check_tag_commit "$tag_commit"
if [[ -z "$tag_commit" ]]; then
  echo "Tag $RELEASE_TAG disappeared before publication" >&2
  exit 1
fi
gh release edit "$RELEASE_TAG" --draft=false
