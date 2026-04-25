# Repository Guidelines

## Project Structure

This repository contains an Emacs completion style backed by a Rust dynamic
module.

- `nucleo-completion.el`: Emacs Lisp completion style, module loader, fallback filtering, regexp expander support, and highlighting.
- `src/lib.rs`: Rust dynamic module exported to Emacs via the `emacs` crate.
- `test/nucleo-completion-tests.el`: ERT tests.
- `.github/workflows/prebuilt-modules.yml`: cross-platform release artifact builds.
- `README.org`: user-facing setup, build, and extension notes.

## Build And Test

Run commands from the repository root.

- `cargo build --release`: build the Rust dynamic module.
- `emacs -Q --batch -L . -L target/release --eval '(byte-compile-file "nucleo-completion.el")'`: byte-compile the Elisp package.
- `emacs -Q --batch -L . -L target/release -L test -l test/nucleo-completion-tests.el -f ert-run-tests-batch-and-exit`: run the test suite.

When changing only documentation, tests are optional. When changing Elisp or
Rust behavior, run all three commands.

## Coding Style

Use standard Emacs Lisp style: lexical binding, two-space indentation, docstrings
for public functions and customization variables, and package-prefixed names.
Public symbols must use `nucleo-completion-`; internal helpers must use
`nucleo-completion--`.

Keep Migemo, pyim, and other language-specific integrations out of the package
body. Add only generic hooks in code, and document concrete integrations as user
configuration examples in `README.org`.

Use standard Rust formatting and keep the dynamic module API narrow. Prefer
batching candidate lists across the Elisp/Rust boundary instead of per-candidate
calls.

## Generated Files

Do not commit generated files:

- `target/`
- `*.elc`
- local editor state

Prebuilt dynamic modules may be committed only when intentionally preparing a
binary distribution, and must live under `bin/<target-triple>/`.

## Release Notes

If module loading, platform detection, build targets, or user configuration
changes, update `README.org`. If GitHub Actions output changes, update both
`README.org` and `.github/workflows/prebuilt-modules.yml` together.
