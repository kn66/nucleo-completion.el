;;; nucleo-completion.el --- Nucleo-backed completion style  -*- lexical-binding: t -*-

;; Copyright (C) 2026

;; Author: Nobu <https://github.com/kn66>
;; Assisted-by: OpenAI Codex:gpt-5
;; Version: 0.1.14
;; Package-Requires: ((emacs "27.1"))
;; Keywords: matching, convenience
;; URL: https://github.com/kn66/nucleo-completion.el
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Fuzzy completion style backed by the Rust nucleo-matcher crate.
;;
;; Build the dynamic module with:
;;
;;   cargo build --release
;;
;; Then add this directory to `load-path' and prepend `nucleo' to
;; `completion-styles'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function completion--merge-suffix "minibuffer")
(declare-function nucleo-completion-candidates
                  "ext:nucleo-completion-module"
                  (pattern candidates ignore-case sort-ties-by-length
                           sort-ties-alphabetically highlight-limit
                           return-all-scores))
(declare-function nucleo-completion-candidates-with-history
                  "ext:nucleo-completion-module"
                  (pattern candidates ignore-case sort-ties-by-length
                           sort-ties-alphabetically history-ranks
                           highlight-limit return-all-scores))
(declare-function nucleo-completion-module-version
                  "ext:nucleo-completion-module")
(declare-function mm-destroy-parts "mm-decode")
(declare-function mm-dissect-buffer "mm-decode")
(declare-function mm-save-part-to-file "mm-decode")
(declare-function url-retrieve-synchronously "url")
(defvar mm-attachment-file-modes)
(defvar completion-flex-nospace)
(defvar completion-lazy-hilit nil
  "Non-nil when completion UIs request lazy candidate highlighting.")
(defvar completion-lazy-hilit-fn)
(defvar completion-regexp-list)
(defvar corfu-history)
(defvar corfu-history-mode)
(defvar minibuffer-default)
(defvar minibuffer-history-variable)
(defvar url-http-codes)
(defvar url-http-response-status)

(defgroup nucleo-completion nil
  "Nucleo-backed fuzzy completion style."
  :group 'minibuffer)

(defcustom nucleo-completion-max-highlighted-completions 25
  "Number of top-ranking completions to highlight eagerly.
Large values can decrease performance."
  :type 'natnum
  :group 'nucleo-completion)

(defcustom nucleo-completion-regexp-functions nil
  "Functions used to expand each search term to extra regexps.
Each function is called with one whitespace-separated search term.
It may return a regexp string, a list of regexp strings, or nil.

The built-in fuzzy subsequence matcher is always used.  Regexps
from this option are ORed with it for each term, while terms are
ANDed together."
  :type '(repeat function)
  :group 'nucleo-completion)

(defcustom nucleo-completion-report-regexp-function-errors nil
  "Whether to warn when a regexp expander signals an error.
When nil, failing functions in `nucleo-completion-regexp-functions'
are ignored.  When non-nil, each failing function is reported once
with `display-warning' and then ignored for that call."
  :type 'boolean
  :group 'nucleo-completion)

(defcustom nucleo-completion-regexp-minimum-term-length 2
  "Minimum search term length for calling regexp expanders.
Terms shorter than this value use only the built-in fuzzy matcher.
The default avoids broad one-character regexp expansions from tools
such as Migemo.  Set this to 1 to run regexp expanders for
single-character terms too."
  :type 'natnum
  :group 'nucleo-completion)

(defcustom nucleo-completion-regexp-only-match-priority 'non-ascii
  "How to order matches found only by configured regexp expanders.
The value `non-ascii' promotes regexp-only candidates containing
non-ASCII characters before Nucleo-scored fuzzy matches and
places ASCII-only regexp-only candidates after them.  This keeps
cross-script dictionary matches prominent while preventing broad
ASCII regexp expansions from outranking direct fuzzy matches.

The value `before' promotes all regexp-only matches before
Nucleo-scored fuzzy matches, matching earlier versions'
behavior.  The value `after' places all regexp-only matches after
Nucleo-scored fuzzy matches."
  :type '(choice (const :tag "Promote non-ASCII regexp-only matches" non-ascii)
                 (const :tag "Promote all regexp-only matches" before)
                 (const :tag "Append all regexp-only matches" after))
  :group 'nucleo-completion)

(defcustom nucleo-completion-scrub-non-unicode-candidates nil
  "Whether to strip non-Unicode codepoints before calling the Rust module.
Some frontends append disambiguation characters above Unicode's
maximum codepoint to completion candidates.  The Rust module
cannot encode those characters as UTF-8, so enabling this option
walks every candidate, sends scrubbed copies to Rust when needed,
and restores the original strings on return.

Keep this nil when candidates are ordinary Unicode strings.  It
avoids an extra Emacs Lisp scan over every candidate before each
module call.  If a module call later fails with a
`unicode-string-p' error, Nucleo retries that call once with
scrubbing enabled.  Set this option explicitly when a frontend
regularly returns non-Unicode candidates."
  :type 'boolean
  :group 'nucleo-completion)

(defvar nucleo-completion--force-scrub-non-unicode-candidates nil
  "Non-nil to force non-Unicode scrubbing for module calls.
This internal flag is not set automatically after retryable module
errors; use `nucleo-completion-scrub-non-unicode-candidates' for
persistent opt-in scrubbing.")

(defcustom nucleo-completion-sort-ties-by-history nil
  "Whether to sort equal-scoring matches by completion history.
When non-nil, candidates found earlier in the current history
source come first.  Minibuffers use the active minibuffer history
variable.  Ordinary buffers use `corfu-history' when
`corfu-history-mode' is enabled.  With the Rust module, this only
affects candidates with the same Nucleo score.  In the Emacs Lisp
fallback, all matches are treated as equal-scoring."
  :type 'boolean
  :group 'nucleo-completion)

(defcustom nucleo-completion-sort-ties-by-length nil
  "Whether to sort equal-scoring matches by candidate length.
When non-nil, shorter candidates come first.  With the Rust
module, this only affects candidates with the same Nucleo score.
In the Emacs Lisp fallback, all matches are treated as
equal-scoring."
  :type 'boolean
  :group 'nucleo-completion)

(defcustom nucleo-completion-sort-ties-alphabetically nil
  "Whether to sort equal-scoring matches alphabetically.
When `nucleo-completion-sort-ties-by-length' is also non-nil,
alphabetical order is used after comparing length.  With the Rust
module, this only affects candidates with the same Nucleo score.
In the Emacs Lisp fallback, all matches are treated as
equal-scoring."
  :type 'boolean
  :group 'nucleo-completion)

(defcustom nucleo-completion-highlight-score-bands nil
  "Whether to highlight high- and low-scoring completion candidates.
When non-nil, candidates with exact word matches or scores close to
the best score use `nucleo-completion-high-score-face'.  Other
scored candidates use `nucleo-completion-low-score-face'."
  :type 'boolean
  :group 'nucleo-completion)

(defcustom nucleo-completion-high-score-ratio 0.85
  "Minimum ratio to the best score for high-score candidate highlighting.
For example, 0.85 means candidates whose score is at least 85% of
the best score are highlighted with
`nucleo-completion-high-score-face'.  Exact word matches are always
treated as high-scoring candidates."
  :type '(number :tag "Ratio")
  :group 'nucleo-completion)

(defcustom nucleo-completion-high-score-emphasis '(bold underline)
  "Additional emphasis faces for high-score candidate highlighting.
The value is a list containing `bold', `underline', both, or nil.
This option only affects high-score candidates when
`nucleo-completion-highlight-score-bands' is non-nil."
  :type '(set (const :tag "Bold" bold)
              (const :tag "Underline" underline))
  :group 'nucleo-completion)

(defcustom nucleo-completion-report-module-load-errors nil
  "Whether to report dynamic module load failures with `message'.
Load failures are always saved in
`nucleo-completion-module-load-errors'."
  :type 'boolean
  :group 'nucleo-completion)

(defcustom nucleo-completion-module-directory
  (expand-file-name "nucleo-completion/modules/" user-emacs-directory)
  "Directory where `nucleo-completion-install-module' stores modules."
  :type 'directory
  :group 'nucleo-completion)

(defcustom nucleo-completion-module-release-repository
  "kn66/nucleo-completion.el"
  "GitHub OWNER/REPO used by `nucleo-completion-install-module'."
  :type 'string
  :group 'nucleo-completion)

(defconst nucleo-completion-required-module-version "0.1.14"
  "Version of the prebuilt Rust module expected by this package.")

(defcustom nucleo-completion-module-release-tag nil
  "GitHub Release tag used by `nucleo-completion-install-module'.
When nil, the installer downloads the release matching
`nucleo-completion-required-module-version'.  The value `latest'
uses GitHub's latest release URL."
  :type '(choice (const :tag "Package-matched release" nil)
                 (const :tag "Latest release" latest)
                 (string :tag "Release tag"))
  :group 'nucleo-completion)

(defcustom nucleo-completion-module-install-policy 'manual
  "How to handle a missing Rust dynamic module.
The value `manual' never prompts automatically; users can run
`nucleo-completion-install-module' explicitly.  The value `prompt'
asks once in interactive sessions when no module is available.  Nil
never prompts."
  :type '(choice (const :tag "Manual command only" manual)
                 (const :tag "Ask once when loaded interactively" prompt)
                 (const :tag "Never ask" nil))
  :group 'nucleo-completion)

(defface nucleo-completion-high-score-face
  '((t nil))
  "Face used for candidates in the high score band."
  :group 'nucleo-completion)

(defface nucleo-completion-low-score-face
  '((t (:inherit shadow)))
  "Face used for candidates below the high score band."
  :group 'nucleo-completion)

(defvar-local nucleo-completion--filtering-p nil
  "Non-nil while Nucleo is actively filtering completion candidates.")

(defvar-local nucleo-completion--current-result nil
  "Most recent unbased completion result produced during filtering.")

(defvar-local nucleo-completion--current-prefix ""
  "Prefix corresponding to `nucleo-completion--current-result'.")

(defvar-local nucleo-completion--current-base-size nil
  "Completion table base-size for `nucleo-completion--current-result'.")

(defvar nucleo-completion-module-load-errors nil
  "Dynamic module load failures from the last load attempt.
Each entry has the form (FILE . MESSAGE).")

(defvar nucleo-completion--loaded-module-file nil
  "File name of the dynamic module loaded by `nucleo-completion--load-module'.")

(defvar nucleo-completion--module-install-prompted nil
  "Non-nil after an automatic module install prompt has been considered.")

(defvar nucleo-completion--stale-module-warning-shown nil
  "Non-nil after warning about an older installed dynamic module.")

(defvar nucleo-completion--module-version-warning-shown nil
  "Non-nil after warning about an incompatible loaded dynamic module.")

(defvar nucleo-completion--regexp-function-error-warnings nil
  "Regexp expander functions already reported for signaling errors.")

(defconst nucleo-completion--prebuilt-release-triples
  '("x86_64-unknown-linux-gnu"
    "x86_64-apple-darwin"
    "aarch64-apple-darwin"
    "x86_64-pc-windows-msvc")
  "Target triples currently published as GitHub Release assets.")

(defvar nucleo-completion--regexp-cache nil
  "Cache for regexp expander results during one completion pass.")

(defvar nucleo-completion--terms-cache nil
  "Cache for split search terms during one completion pass.")

(defvar nucleo-completion--subsequence-regexp-cache nil
  "Cache for fuzzy subsequence regexps during one completion pass.")

(defvar nucleo-completion--exact-word-regexp-cache nil
  "Cache for exact word regexps during one completion pass.")

(defconst nucleo-completion--score-property 'nucleo-completion-score
  "Text property used to carry Rust scores on returned candidates.")

(defconst nucleo-completion--interrupted-sentinel
  'nucleo-completion-interrupted
  "Marker used by the Rust module when scoring was interrupted.")

(defconst nucleo-completion--scrub-map-pairs-key
  'nucleo-completion--scrub-map-pairs
  "Private key for ordered scrubbed-candidate restore pairs.")

(defconst nucleo-completion--directory
  (file-name-directory (or load-file-name byte-compile-current-file buffer-file-name))
  "Directory containing nucleo-completion.el.")

(defun nucleo-completion--nonnegative-integer-or-nil (value)
  "Return VALUE when it is a nonnegative integer, otherwise nil."
  (when (and (integerp value) (>= value 0))
    value))

(defun nucleo-completion--cached (cache key producer)
  "Return CACHE value for KEY, computing it with PRODUCER when absent.
When CACHE is not a hash table, call PRODUCER without caching."
  (if (hash-table-p cache)
      (let ((cached (gethash key cache :missing)))
        (if (not (eq cached :missing))
            cached
          (let ((value (funcall producer)))
            (puthash key value cache)
            value)))
    (funcall producer)))

(defun nucleo-completion--rust-library-name ()
  "Return Cargo's dynamic library file name for this platform."
  (pcase system-type
    ('windows-nt "nucleo_completion_module.dll")
    ('darwin "libnucleo_completion_module.dylib")
    (_ "libnucleo_completion_module.so")))

(defun nucleo-completion--platform-triples ()
  "Return likely target triples for the current Emacs."
  (let ((config (or system-configuration "")))
    (pcase system-type
      ('darwin
       (if (string-match-p "\\(?:aarch64\\|arm64\\)" config)
           '("aarch64-apple-darwin")
         '("x86_64-apple-darwin")))
      ('windows-nt
       (cond
        ((string-match-p "aarch64\\|arm64" config)
         '("aarch64-pc-windows-msvc"))
        ((string-match-p "mingw\\|gnu" config)
         '("x86_64-pc-windows-gnu" "x86_64-pc-windows-msvc"))
        (t
         '("x86_64-pc-windows-msvc" "x86_64-pc-windows-gnu"))))
      ('gnu/linux
       (cond
        ((string-match-p "aarch64\\|arm64" config)
         '("aarch64-unknown-linux-gnu" "aarch64-unknown-linux-musl"))
        (t
         '("x86_64-unknown-linux-gnu" "x86_64-unknown-linux-musl"))))
      (_ nil))))

(defun nucleo-completion--prebuilt-module-directories ()
  "Return prebuilt module directories for the current platform."
  (mapcar (lambda (triple)
            (expand-file-name (concat "bin/" triple) nucleo-completion--directory))
          (nucleo-completion--platform-triples)))

(defun nucleo-completion--required-module-release-tag ()
  "Return the GitHub Release tag matching this package's module version."
  (if (string-prefix-p "v" nucleo-completion-required-module-version)
      nucleo-completion-required-module-version
    (concat "v" nucleo-completion-required-module-version)))

(defun nucleo-completion--module-latest-release-p ()
  "Return non-nil when the installer should use GitHub's latest release URL."
  (eq nucleo-completion-module-release-tag 'latest))

(defun nucleo-completion--configured-module-release-tag ()
  "Return the concrete GitHub Release tag selected for module install."
  (if (and (stringp nucleo-completion-module-release-tag)
           (not (string-empty-p nucleo-completion-module-release-tag)))
      nucleo-completion-module-release-tag
    (nucleo-completion--required-module-release-tag)))

(defun nucleo-completion--module-release-directory-name ()
  "Return the local directory name for the configured module release."
  (if (nucleo-completion--module-latest-release-p)
      "latest"
    (nucleo-completion--configured-module-release-tag)))

(defun nucleo-completion--module-install-triple ()
  "Return the current platform triple supported by release assets."
  (cl-find-if
   (lambda (triple)
     (member triple nucleo-completion--prebuilt-release-triples))
   (nucleo-completion--platform-triples)))

(defun nucleo-completion--module-install-directory (triple)
  "Return the local module install directory for TRIPLE."
  (file-name-as-directory
   (expand-file-name
    triple
    (expand-file-name
     (nucleo-completion--module-release-directory-name)
     (file-name-as-directory nucleo-completion-module-directory)))))

(defun nucleo-completion--stale-installed-module-directories ()
  "Return installed module directories for older configured releases."
  (let ((triple (nucleo-completion--module-install-triple)))
    (when (and triple
               (file-directory-p nucleo-completion-module-directory))
      (let* ((library (nucleo-completion--rust-library-name))
             (current (directory-file-name
                       (nucleo-completion--module-install-directory triple))))
        (cl-loop for release-dir in
                 (directory-files nucleo-completion-module-directory t "\\`[^.]")
                 for module-dir = (expand-file-name triple release-dir)
                 for module-file = (expand-file-name library module-dir)
                 when (and (file-readable-p module-file)
                           (not (string= (expand-file-name module-dir)
                                         (expand-file-name current))))
                 collect module-dir)))))

(defun nucleo-completion--installed-module-directories ()
  "Return installed module directories for the current platform."
  (let ((triple (nucleo-completion--module-install-triple)))
    (when triple
      (list (nucleo-completion--module-install-directory triple)))))

(defun nucleo-completion--installed-module-release-name (file)
  "Return the local release directory name for installed module FILE.
Return nil when FILE is not under `nucleo-completion-module-directory'."
  (let* ((file (expand-file-name file))
         (triple-dir (file-name-directory file))
         (release-dir (and triple-dir
                           (file-name-directory
                            (directory-file-name triple-dir))))
         (base-dir (and release-dir
                        (file-name-directory
                         (directory-file-name release-dir))))
         (release-name (and release-dir
                            (file-name-nondirectory
                             (directory-file-name release-dir)))))
    (when (and base-dir
               (string= (file-name-as-directory (expand-file-name base-dir))
                        (file-name-as-directory
                         (expand-file-name nucleo-completion-module-directory))))
      release-name)))

(defun nucleo-completion--installed-module-stale-p (file)
  "Return non-nil when installed module FILE is not for the configured release."
  (let ((release (nucleo-completion--installed-module-release-name file)))
    (and release
         (not (string= release
                       (nucleo-completion--module-release-directory-name))))))

(defun nucleo-completion--loaded-installed-module-stale-p ()
  "Return non-nil when the loaded installed module is for an older release."
  (and nucleo-completion--loaded-module-file
       (nucleo-completion--installed-module-stale-p
        nucleo-completion--loaded-module-file)))

(defun nucleo-completion--loaded-module-version ()
  "Return the loaded Rust module version string, or nil."
  (when (fboundp 'nucleo-completion-module-version)
    (let ((version (ignore-errors (nucleo-completion-module-version))))
      (and (stringp version) version))))

(defun nucleo-completion--loaded-module-version-compatible-p ()
  "Return non-nil when the loaded Rust module version is compatible.
Test doubles that define the candidate API without loading the
dynamic module are accepted so unit tests can exercise the module
call path without a built binary."
  (and (fboundp 'nucleo-completion-candidates)
       (let ((version (nucleo-completion--loaded-module-version)))
         (if version
             (string= version nucleo-completion-required-module-version)
           (not (featurep 'nucleo-completion-module))))))

(defun nucleo-completion--maybe-warn-incompatible-loaded-module ()
  "Warn once when the loaded Rust module version is incompatible."
  (when (and (not nucleo-completion--module-version-warning-shown)
             (fboundp 'nucleo-completion-candidates)
             (not (nucleo-completion--loaded-module-version-compatible-p)))
    (setq nucleo-completion--module-version-warning-shown t)
    (display-warning
     'nucleo-completion
     (format
      (concat
       "Loaded Rust module version %s, but this package expects %s. "
       "Install the matching module and restart Emacs.")
      (or (nucleo-completion--loaded-module-version) "unknown")
      nucleo-completion-required-module-version)
     :warning)))

(defun nucleo-completion--current-installed-module-file ()
  "Return the current configured installed module file, or nil."
  (let ((triple (nucleo-completion--module-install-triple)))
    (when triple
      (expand-file-name
       (nucleo-completion--rust-library-name)
       (nucleo-completion--module-install-directory triple)))))

(defun nucleo-completion--maybe-warn-stale-loaded-module ()
  "Warn once when an older installed dynamic module was loaded."
  (when (and (not nucleo-completion--stale-module-warning-shown)
             (nucleo-completion--loaded-installed-module-stale-p))
    (setq nucleo-completion--stale-module-warning-shown t)
    (display-warning
     'nucleo-completion
     (format
      (concat
       "Loaded module release %s, but this package expects module release %s. "
       "Run M-x nucleo-completion-install-module and restart Emacs.")
      (nucleo-completion--installed-module-release-name
       nucleo-completion--loaded-module-file)
      (nucleo-completion--module-release-directory-name))
     :warning)))

(defun nucleo-completion--dynamic-modules-supported-p ()
  "Return non-nil when this Emacs can load dynamic modules."
  (and (fboundp 'module-load)
       (boundp 'module-file-suffix)))

(defun nucleo-completion--module-candidates ()
  "Return candidate paths for the Rust dynamic module."
  (when (nucleo-completion--dynamic-modules-supported-p)
    (let ((library (nucleo-completion--rust-library-name)))
      (delete-dups
       (delq nil
             (append
              (mapcar (lambda (dir)
                        (expand-file-name library dir))
                      (append
                       (nucleo-completion--installed-module-directories)
                       (nucleo-completion--prebuilt-module-directories)
                       (list nucleo-completion--directory
                             (expand-file-name "target/release"
                                               nucleo-completion--directory)
                             (expand-file-name "target/debug"
                                               nucleo-completion--directory))))
              (list
               (locate-library "nucleo-completion-module"
                               nil (list module-file-suffix))
               (locate-library library))
              (mapcar (lambda (dir)
                        (expand-file-name library dir))
                      (nucleo-completion--stale-installed-module-directories))))))))

(defun nucleo-completion--highlight-limit ()
  "Return a sanitized value for eager or module highlight limits."
  (or (nucleo-completion--nonnegative-integer-or-nil
       nucleo-completion-max-highlighted-completions)
      0))

(defun nucleo-completion--regexp-minimum-term-length ()
  "Return a sanitized minimum term length for regexp expanders."
  (or (nucleo-completion--nonnegative-integer-or-nil
       nucleo-completion-regexp-minimum-term-length)
      2))

(defun nucleo-completion--load-module ()
  "Load the Rust dynamic module when it is available."
  (when (and (nucleo-completion--dynamic-modules-supported-p)
             (not (featurep 'nucleo-completion-module)))
    (setq nucleo-completion-module-load-errors nil)
    (let ((loaded nil))
      (catch 'loaded
        (dolist (file (nucleo-completion--module-candidates))
          (when (and file (file-readable-p file))
            (condition-case err
                (progn
                  (module-load file)
                  (setq loaded t
                        nucleo-completion--loaded-module-file file)
                  (nucleo-completion--maybe-warn-stale-loaded-module)
                  (nucleo-completion--maybe-warn-incompatible-loaded-module)
                  (throw 'loaded t))
              (error
               (push (cons file (error-message-string err))
                     nucleo-completion-module-load-errors))))))
      (setq nucleo-completion-module-load-errors
            (nreverse nucleo-completion-module-load-errors))
      (when (and (not loaded)
                 nucleo-completion-report-module-load-errors
                 nucleo-completion-module-load-errors)
        (message
         "nucleo-completion: failed to load dynamic module: %s"
         (mapconcat
          (lambda (entry)
            (format "%s: %s" (car entry) (cdr entry)))
          nucleo-completion-module-load-errors
          "; "))))))

(nucleo-completion--load-module)

(defun nucleo-completion--module-asset-extension ()
  "Return the filename extension for the current platform module asset."
  (file-name-extension (nucleo-completion--rust-library-name)))

(defun nucleo-completion--module-asset-name (triple)
  "Return the GitHub Release asset name for TRIPLE."
  (format "nucleo-completion-module-%s.%s"
          triple
          (nucleo-completion--module-asset-extension)))

(defun nucleo-completion--module-release-base-url ()
  "Return the GitHub Release download base URL."
  (format "https://github.com/%s/releases/%s"
          nucleo-completion-module-release-repository
          (if (nucleo-completion--module-latest-release-p)
              "latest/download"
            (concat "download/"
                    (nucleo-completion--configured-module-release-tag)))))

(defun nucleo-completion--module-asset-url (triple &optional checksum)
  "Return the GitHub Release URL for TRIPLE.
When CHECKSUM is non-nil, return the SHA256 checksum asset URL."
  (concat (nucleo-completion--module-release-base-url)
          "/"
          (nucleo-completion--module-asset-name triple)
          (when checksum ".sha256")))

(defun nucleo-completion--download-file (url file)
  "Download URL to FILE, replacing FILE if it exists."
  (require 'url)
  (require 'mm-decode)
  (let ((buffer (url-retrieve-synchronously url t t)))
    (unless buffer
      (error "Failed to download %s" url))
    (unwind-protect
        (with-current-buffer buffer
          (let ((status (and (boundp 'url-http-response-status)
                             url-http-response-status)))
            (unless (or (not status)
                        (and (>= status 200) (< status 300))
                        (= status 304))
              (let ((desc (and (boundp 'url-http-codes)
                               (nth 2 (assq status url-http-codes)))))
                (error "Failed to download %s: HTTP %s%s"
                       url status (if desc (format " %s" desc) "")))))
          (let ((handle (mm-dissect-buffer t))
                (mm-attachment-file-modes (default-file-modes)))
            (unwind-protect
                (mm-save-part-to-file handle file)
              (mm-destroy-parts handle))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun nucleo-completion--sha256-from-file (file)
  "Return the first SHA256 digest found in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (unless (re-search-forward "\\b[0-9a-fA-F]\\{64\\}\\b" nil t)
      (error "No SHA256 digest found in %s" file))
    (downcase (match-string 0))))

(defun nucleo-completion--file-sha256 (file)
  "Return the SHA256 digest of FILE contents."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))

(defun nucleo-completion--verify-sha256 (file checksum-file)
  "Signal unless FILE matches the SHA256 digest in CHECKSUM-FILE."
  (let ((expected (nucleo-completion--sha256-from-file checksum-file))
        (actual (nucleo-completion--file-sha256 file)))
    (unless (string= expected actual)
      (error "SHA256 mismatch for %s: expected %s, got %s"
             file expected actual))))

(defun nucleo-completion--terms (pattern)
  "Split PATTERN into non-empty whitespace-separated terms."
  (nucleo-completion--cached
   nucleo-completion--terms-cache
   pattern
   (lambda () (split-string pattern "[[:space:]]+" t))))

(defun nucleo-completion--flex-nospace-p (pattern)
  "Return non-nil when built-in flex settings reject PATTERN."
  (and (bound-and-true-p completion-flex-nospace)
       (string-match-p "[[:space:]]" pattern)))

(defun nucleo-completion--subsequence-regexp (needle)
  "Return a fuzzy subsequence regexp for NEEDLE."
  (nucleo-completion--cached
   nucleo-completion--subsequence-regexp-cache
   needle
   (lambda () (nucleo-completion--subsequence-regexp-1 needle))))

(defun nucleo-completion--subsequence-regexp-1 (needle)
  "Return an uncached fuzzy subsequence regexp for NEEDLE."
  (mapconcat
   (lambda (ch)
     (concat "\\(?:.\\|\n\\)*" (regexp-quote (char-to-string ch))))
   needle
   ""))

(defun nucleo-completion--valid-regexp-p (regexp)
  "Return non-nil when REGEXP is a valid regexp string."
  (and (stringp regexp)
       (not (string-empty-p regexp))
       (condition-case nil
           (progn
             (string-match-p regexp "")
             t)
         (invalid-regexp nil))))

(defun nucleo-completion--maybe-warn-regexp-function-error
    (function term error-message)
  "Warn once that FUNCTION failed for TERM with ERROR-MESSAGE."
  (when (and nucleo-completion-report-regexp-function-errors
             (not (memq function
                        nucleo-completion--regexp-function-error-warnings)))
    (push function nucleo-completion--regexp-function-error-warnings)
    (display-warning
     'nucleo-completion
     (format "Regexp expander %S failed for term %S: %s"
             function term error-message)
     :warning)))

(defun nucleo-completion--regexp-function-value (function term)
  "Return FUNCTION's regexp expansion for TERM, or nil on error."
  (when (functionp function)
    (condition-case err
        (funcall function term)
      (error
       (nucleo-completion--maybe-warn-regexp-function-error
        function term (error-message-string err))
       nil))))

(defun nucleo-completion--regexp-function-regexps (term)
  "Return extra regexps produced for TERM."
  (nucleo-completion--cached
   nucleo-completion--regexp-cache
   term
   (lambda () (nucleo-completion--regexp-function-regexps-uncached term))))

(defun nucleo-completion--regexp-function-regexps-uncached (term)
  "Return uncached extra regexps produced for TERM."
  (when (>= (length term) (nucleo-completion--regexp-minimum-term-length))
    (cl-loop for function in nucleo-completion-regexp-functions
             for value = (nucleo-completion--regexp-function-value
                          function term)
             append (cond
                     ((nucleo-completion--valid-regexp-p value)
                      (list value))
                     ((listp value)
                      (cl-remove-if-not
                       #'nucleo-completion--valid-regexp-p value))))))

(defun nucleo-completion--term-fuzzy-regexp (term)
  "Return the fuzzy subsequence regexp for TERM."
  (concat "\\`" (nucleo-completion--subsequence-regexp term)))

(defun nucleo-completion--term-regexps (term)
  "Return regexps that may match TERM."
  (let ((fuzzy (nucleo-completion--term-fuzzy-regexp term)))
    (if nucleo-completion-regexp-functions
        (cons fuzzy (nucleo-completion--regexp-function-regexps term))
      (list fuzzy))))

(defun nucleo-completion--term-regexp-groups (needle)
  "Return regexp groups for NEEDLE.
Each group corresponds to one term.  A candidate must match at
least one regexp from every group."
  (mapcar (lambda (term)
            (nucleo-completion--term-regexps term))
          (nucleo-completion--terms needle)))

(defun nucleo-completion--expanded-regexp-p (needle)
  "Return non-nil if NEEDLE has extra regexps from configured functions."
  (and nucleo-completion-regexp-functions
       (cl-some #'nucleo-completion--regexp-function-regexps
                (nucleo-completion--terms needle))))

(defun nucleo-completion--regexp-only-term-regexps (term)
  "Return regexps for TERM in the module regexp-only pass.
When TERM has configured regexp expansions, return only those
regexps because the Rust module has already handled fuzzy matching.
Terms without configured expansions still use the fuzzy matcher so
they can combine with expanded terms."
  (or (nucleo-completion--regexp-function-regexps term)
      (list (nucleo-completion--term-fuzzy-regexp term))))

(defun nucleo-completion--regexp-only-regexp-groups (needle)
  "Return regexp groups for preserving regexp-only matches to NEEDLE."
  (mapcar #'nucleo-completion--regexp-only-term-regexps
          (nucleo-completion--terms needle)))

(defun nucleo-completion--regexp-match-p (regexp-groups candidate)
  "Return non-nil if CANDIDATE matches REGEXP-GROUPS."
  (let ((groups regexp-groups)
        (matched t))
    (while (and matched groups)
      (let ((regexps (car groups))
            group-matched)
        (while (and (not group-matched) regexps)
          (when (string-match-p (car regexps) candidate)
            (setq group-matched t))
          (setq regexps (cdr regexps)))
        (setq matched group-matched
              groups (cdr groups))))
    matched))

(defun nucleo-completion--regexp-filter (needle candidates)
  "Filter CANDIDATES against NEEDLE with fuzzy and configured regexp matchers."
  (let ((case-fold-search completion-ignore-case)
        (regexp-groups (nucleo-completion--term-regexp-groups needle)))
    (cl-loop for candidate in candidates
             when (nucleo-completion--regexp-match-p
                   regexp-groups candidate)
             collect candidate)))

(defun nucleo-completion--fallback-alphabetical-key (candidate)
  "Return CANDIDATE's alphabetical sort key for fallback sorting."
  (let ((key (substring-no-properties candidate)))
    (if completion-ignore-case
        (downcase key)
      key)))

(defun nucleo-completion--fallback-sort-before-p
    (item-a item-b rank-table)
  "Return non-nil when ITEM-A should sort before ITEM-B.
RANK-TABLE maps candidate strings to history ranks.  Items have
the form [CANDIDATE POSITION LENGTH ALPHABETICAL-KEY]."
  (let* ((candidate-a (aref item-a 0))
         (candidate-b (aref item-b 0))
         (position-a (aref item-a 1))
         (position-b (aref item-b 1))
         (length-a (aref item-a 2))
         (length-b (aref item-b 2))
         (key-a (aref item-a 3))
         (key-b (aref item-b 3))
         (history-order
          (and rank-table
               (nucleo-completion--history-rank-order
                candidate-a candidate-b rank-table))))
    (cond
     ((eq history-order 'before) t)
     ((eq history-order 'after) nil)
     ((and nucleo-completion-sort-ties-by-length
           (/= length-a length-b))
      (< length-a length-b))
     ((and nucleo-completion-sort-ties-alphabetically
           (not (string= key-a key-b)))
      (string< key-a key-b))
     (t
      (< position-a position-b)))))

(defun nucleo-completion--fallback-sort (candidates prefix)
  "Sort fallback CANDIDATES as equal-score matches under PREFIX."
  (if (or (null candidates) (null (cdr candidates)))
      candidates
    (let ((rank-table (nucleo-completion--history-rank-table prefix)))
      (if (not (or rank-table
                   nucleo-completion-sort-ties-by-length
                   nucleo-completion-sort-ties-alphabetically))
          candidates
        (mapcar
         (lambda (item) (aref item 0))
         (sort
          (cl-loop for candidate in candidates
                   for position from 0
                   collect
                   (vector candidate
                           position
                           (and nucleo-completion-sort-ties-by-length
                                (length candidate))
                           (and nucleo-completion-sort-ties-alphabetically
                                (nucleo-completion--fallback-alphabetical-key
                                 candidate))))
          (lambda (item-a item-b)
            (nucleo-completion--fallback-sort-before-p
             item-a item-b rank-table))))))))

(defun nucleo-completion--fallback-filter
    (needle candidates &optional prefix)
  "Filter CANDIDATES against NEEDLE and sort fallback ties.
PREFIX is used to interpret minibuffer history entries.  The
fallback does not compute scores, so every filtered candidate is
treated as equal-scoring for tie sorting."
  (nucleo-completion--fallback-sort
   (nucleo-completion--regexp-filter needle candidates)
   prefix))

(defun nucleo-completion--module-filter (needle candidates ignore-case)
  "Filter and sort CANDIDATES against NEEDLE with the Rust module.
Honor IGNORE-CASE."
  (let ((completion-ignore-case ignore-case))
    (car (nucleo-completion--module-completion-results
          "" needle candidates nil 0 nil))))

(defun nucleo-completion--candidates-with-scores (candidates scores)
  "Return CANDIDATES paired with parallel SCORES.
When SCORES is shorter than CANDIDATES, keep the remaining
candidates paired with nil scores."
  (let ((scores scores)
        result)
    (dolist (candidate candidates (nreverse result))
      (push (cons candidate (car scores)) result)
      (when scores
        (setq scores (cdr scores))))))

(defun nucleo-completion--module-filter-with-scores
    (needle candidates ignore-case)
  "Filter CANDIDATES against NEEDLE with the Rust module and keep scores.
Honor IGNORE-CASE.  The result is an alist of (CANDIDATE . SCORE)."
  (let ((completion-ignore-case ignore-case))
    (pcase-let ((`(,cands ,_bundle ,_top-info ,scores)
                 (nucleo-completion--module-completion-results
                  "" needle candidates nil 0 t)))
      (nucleo-completion--candidates-with-scores cands scores))))

(defun nucleo-completion--module-ready-p ()
  "Return non-nil when the current Rust module provides a compatible API."
  (prog1 (nucleo-completion--loaded-module-version-compatible-p)
    (nucleo-completion--maybe-warn-incompatible-loaded-module)))

(defun nucleo-completion--module-supports-history-p ()
  "Return non-nil when the Rust module supports history tie sorting."
  (and (nucleo-completion--module-ready-p)
       (fboundp 'nucleo-completion-candidates-with-history)))

;;;###autoload
(defun nucleo-completion-install-module (&optional force no-confirm)
  "Download and install the Rust dynamic module for this platform.
The module is downloaded from GitHub Releases only after
confirmation, unless NO-CONFIRM is non-nil.  The default release
matches `nucleo-completion-required-module-version'.  With prefix
argument FORCE, replace an existing installed module."
  (interactive "P")
  (unless (nucleo-completion--dynamic-modules-supported-p)
    (user-error "This Emacs was built without dynamic module support"))
  (let* ((module-loaded (nucleo-completion--module-ready-p))
         (triple (or (nucleo-completion--module-install-triple)
                     (user-error
                      "No prebuilt module is published for %s"
                      system-configuration)))
         (library (nucleo-completion--rust-library-name))
         (directory (nucleo-completion--module-install-directory triple))
         (destination (expand-file-name library directory))
         (asset-url (nucleo-completion--module-asset-url triple))
         (checksum-url (nucleo-completion--module-asset-url triple t)))
    (when (and (file-exists-p destination)
               (not force)
               (not no-confirm))
      (unless (yes-or-no-p
               (format "Replace existing nucleo-completion module at %s? "
                       destination))
        (user-error "Module installation cancelled")))
    (when (and (file-exists-p destination)
               (not force)
               no-confirm)
      (user-error "Module already exists at %s" destination))
    (unless no-confirm
      (unless (yes-or-no-p
               (format
                "Download nucleo-completion module for %s from %s to %s? "
                triple asset-url destination))
        (user-error "Module installation cancelled")))
    (let ((module-temp (make-temp-file "nucleo-completion-module-"))
          (checksum-temp (make-temp-file "nucleo-completion-module-sha256-")))
      (unwind-protect
          (progn
            (nucleo-completion--download-file checksum-url checksum-temp)
            (nucleo-completion--download-file asset-url module-temp)
            (nucleo-completion--verify-sha256 module-temp checksum-temp)
            (make-directory directory t)
            (rename-file module-temp destination t)
            (if module-loaded
                (message
                 "nucleo-completion: installed %s; restart Emacs to use it"
                 destination)
              (nucleo-completion--load-module)
              (if (nucleo-completion--module-ready-p)
                  (message "nucleo-completion: installed and loaded %s"
                           destination)
                (display-warning
                 'nucleo-completion
                 (format
                  "Installed %s, but it could not be loaded. Check load errors."
                  destination)
                 :warning)))
            destination)
        (when (file-exists-p module-temp)
          (delete-file module-temp))
        (when (file-exists-p checksum-temp)
          (delete-file checksum-temp))))))

;;;###autoload
(defun nucleo-completion-ensure-module (&optional force no-confirm)
  "Ensure the Rust dynamic module is installed and loaded.
If the module API is already available from the configured release,
do nothing.  Otherwise, try loading a module from the normal
search paths.  If that still does not make the module API
available, or only an older installed release could be loaded,
download and install a prebuilt module for the current platform.

When called interactively, ask before downloading or replacing a
module.  With prefix argument FORCE, replace an existing installed
module.  Lisp calls install without confirmation because calling
this function is an explicit opt-in; NO-CONFIRM non-nil also skips
confirmation for wrapper commands.

Return non-nil when the module API is available after the attempt."
  (interactive "P")
  (let ((interactive (called-interactively-p 'interactive)))
    (unless (nucleo-completion--module-ready-p)
      (unless (nucleo-completion--dynamic-modules-supported-p)
        (user-error "This Emacs was built without dynamic module support"))
      (nucleo-completion--load-module))
    (when (or (not (nucleo-completion--module-ready-p))
              force
              (nucleo-completion--loaded-installed-module-stale-p))
      (let* ((destination (nucleo-completion--current-installed-module-file))
             (skip-confirm (or no-confirm (not interactive)))
             (replace-existing
              (and skip-confirm destination (file-exists-p destination))))
        (nucleo-completion-install-module
         (or force replace-existing)
         skip-confirm))))
  (nucleo-completion--module-ready-p))

(defun nucleo-completion--maybe-prompt-module-install ()
  "Prompt once to install the Rust module according to user policy."
  (when (and (eq nucleo-completion-module-install-policy 'prompt)
             (not nucleo-completion--module-install-prompted)
             (not noninteractive)
             (not (eq this-command 'nucleo-completion-install-module))
             (not (nucleo-completion--module-ready-p))
             (nucleo-completion--module-install-triple))
    (setq nucleo-completion--module-install-prompted t)
    (condition-case err
        (nucleo-completion-install-module)
      (error
       (display-warning
        'nucleo-completion
        (format "Rust module installation failed: %s"
                (error-message-string err))
        :warning)))))

(defconst nucleo-completion--max-unicode-codepoint #x10FFFF
  "Highest character that the Rust module can encode as UTF-8.
Frontends like Consult append \"tofu\" disambiguation characters
beyond Unicode (codepoints at #x200000 and above), which fail the
module's `copy_string_contents' UTF-8 encoder.  Such characters
must be stripped before crossing the FFI boundary.")

(defun nucleo-completion--unicode-string-p (string)
  "Return non-nil when every character in STRING is a Unicode codepoint."
  (let ((max nucleo-completion--max-unicode-codepoint)
        (i 0)
        (len (length string))
        (ok t))
    (while (and ok (< i len))
      (when (> (aref string i) max)
        (setq ok nil))
      (setq i (1+ i)))
    ok))

(defun nucleo-completion--scrub-non-unicode-string (string)
  "Return STRING with non-Unicode characters removed.
Returns STRING itself when no character needs to be removed so
the caller can detect untouched candidates with `eq'."
  (if (nucleo-completion--unicode-string-p string)
      string
    (let ((max nucleo-completion--max-unicode-codepoint)
          (i 0)
          (len (length string))
          chars)
      (while (< i len)
        (let ((ch (aref string i)))
          (when (<= ch max)
            (push ch chars)))
        (setq i (1+ i)))
      (apply #'string (nreverse chars)))))

(defun nucleo-completion--scrub-candidates
    (candidates &optional scrub-non-unicode)
  "Return (CLEANED . MAP) where CLEANED is suitable for the Rust module.
When SCRUB-NON-UNICODE,
`nucleo-completion-scrub-non-unicode-candidates', or
`nucleo-completion--force-scrub-non-unicode-candidates' is non-nil,
walk CANDIDATES once and replace strings containing characters
above `nucleo-completion--max-unicode-codepoint' with scrubbed
copies.  MAP is an `eq' hash table from each substituted scrubbed
copy to the original candidate.  It also stores an ordered
restore queue for copied module results when clean and scrubbed
candidates collapse to the same text.  When no scrubbing is
requested, or no candidate needs substitution, the original
CANDIDATES list is returned unchanged and MAP is nil."
  (let ((scrub-non-unicode
         (or scrub-non-unicode
             nucleo-completion-scrub-non-unicode-candidates
             nucleo-completion--force-scrub-non-unicode-candidates)))
    (if (not scrub-non-unicode)
        (cons candidates nil)
      (let (map cleaned restore-pairs)
        (dolist (candidate candidates)
          (let ((scrubbed
                 (nucleo-completion--scrub-non-unicode-string candidate)))
            (push scrubbed cleaned)
            (push (cons scrubbed candidate) restore-pairs)
            (unless (eq scrubbed candidate)
              (unless map
                (setq map (make-hash-table :test #'eq)))
              (puthash scrubbed candidate map))))
        (when map
          (puthash nucleo-completion--scrub-map-pairs-key
                   (nreverse restore-pairs) map))
        (cons (if map (nreverse cleaned) candidates) map)))))

(defun nucleo-completion--scrub-map-queues (map)
  "Return fresh equal-keyed restore queues from MAP."
  (let ((pairs (gethash nucleo-completion--scrub-map-pairs-key map))
        (queues (make-hash-table :test #'equal)))
    (dolist (pair pairs)
      (push (cdr pair) (gethash (car pair) queues)))
    (maphash (lambda (key queue)
               (puthash key (nreverse queue) queues))
             queues)
    queues))

(defun nucleo-completion--restore-scrubbed-candidate
    (candidate map &optional queues)
  "Return original candidate for scrubbed CANDIDATE using MAP.
QUEUES, when non-nil, restores copied scrubbed candidates in
their original order."
  (or (gethash candidate map)
      (let ((queue (and queues (gethash candidate queues))))
        (when queue
          (puthash candidate (cdr queue) queues)
          (car queue)))
      candidate))

(defun nucleo-completion--restore-scored-scrubbed-candidate
    (candidate map queues)
  "Return CANDIDATE restored through MAP while preserving its score.
QUEUES restores copied scrubbed candidates in their original
order."
  (let* ((score (nucleo-completion--candidate-score candidate))
         (restored (nucleo-completion--restore-scrubbed-candidate
                    candidate map queues)))
    (if (and score (not (eq restored candidate)))
        (nucleo-completion--propertize-score restored score)
      restored)))

(defun nucleo-completion--restore-bundle-candidates (bundle map)
  "Replace scrubbed candidates inside BUNDLE with originals from MAP.
MAP keys are the substituted plain candidate objects passed to
the Rust module.  Mutates BUNDLE in place because the lists were
freshly returned by the Rust module and are not shared with any
other caller."
  (let ((candidate-queues (nucleo-completion--scrub-map-queues map))
        (top-info-queues (nucleo-completion--scrub-map-queues map)))
    (cl-loop for cell on (nucleo-completion--bundle-candidates bundle)
             do (setcar cell
                        (nucleo-completion--restore-scored-scrubbed-candidate
                         (car cell) map candidate-queues)))
    (dolist (entry (nucleo-completion--bundle-top-info bundle))
      (setcar entry
              (nucleo-completion--restore-scrubbed-candidate
               (car entry) map top-info-queues))))
  bundle)

(defun nucleo-completion--call-module
    (needle candidates ignore-case highlight-limit return-all-scores
            &optional history-ranks)
  "Call the Rust module for NEEDLE and CANDIDATES.
IGNORE-CASE, HIGHLIGHT-LIMIT, RETURN-ALL-SCORES, and optional
HISTORY-RANKS are passed through to the module.  Return a result
bundle."
  (if (and history-ranks
           (nucleo-completion--module-supports-history-p))
      (nucleo-completion-candidates-with-history
       needle candidates ignore-case
       nucleo-completion-sort-ties-by-length
       nucleo-completion-sort-ties-alphabetically
       history-ranks
       highlight-limit
       return-all-scores)
    (nucleo-completion-candidates
     needle candidates ignore-case
     nucleo-completion-sort-ties-by-length
     nucleo-completion-sort-ties-alphabetically
     highlight-limit
     return-all-scores)))

(defun nucleo-completion--module-unicode-error-p (err)
  "Return non-nil when ERR is the module string encoder rejecting input."
  (and (eq (car-safe err) 'wrong-type-argument)
       (eq (cadr err) 'unicode-string-p)))

(defun nucleo-completion--module-results-with-scrub
    (needle candidates ignore-case highlight-limit return-all-scores
            scrub-non-unicode &optional history-ranks)
  "Return module result bundle for NEEDLE, scrubbing CANDIDATES.
IGNORE-CASE, HIGHLIGHT-LIMIT, and RETURN-ALL-SCORES are passed to
the module.  SCRUB-NON-UNICODE selects the candidate scrub path.
HISTORY-RANKS, when non-nil, is parallel to CANDIDATES."
  (pcase-let* ((`(,cleaned . ,map)
                (nucleo-completion--scrub-candidates
                 candidates scrub-non-unicode))
               (bundle
                (nucleo-completion--call-module
                 needle cleaned ignore-case highlight-limit
                 return-all-scores history-ranks)))
    (if (and map (not (nucleo-completion--bundle-interrupted-p bundle)))
        (nucleo-completion--restore-bundle-candidates bundle map)
      bundle)))

(defun nucleo-completion--module-results
    (needle candidates ignore-case highlight-limit
            &optional return-all-scores history-ranks)
  "Return module result bundle for NEEDLE and CANDIDATES.
The bundle is the list (CANDIDATES TOP-INFO FULL-SCORES) returned
by the Rust module:

  CANDIDATES   - matches sorted by descending score.
  TOP-INFO     - list of (CAND SCORE INDICES) entries for at most
                 HIGHLIGHT-LIMIT top-ranking candidates.  INDICES is
                 the highlight index list for CAND when available,
                 otherwise nil.
  FULL-SCORES  - parallel-to-CANDIDATES list of integer scores when
                 RETURN-ALL-SCORES is non-nil; otherwise nil.

Honor IGNORE-CASE.  HISTORY-RANKS, when non-nil, is a list
parallel to CANDIDATES whose integer entries rank candidates by
minibuffer history recency for equal-score tie sorting.

Frontends such as Consult append \"tofu\" disambiguation characters
whose codepoints exceed Unicode's maximum, which the Rust module
cannot encode as UTF-8.  Such candidates are scrubbed of those
characters before being passed to the module when
`nucleo-completion-scrub-non-unicode-candidates' is non-nil.  When
this option is nil and the module rejects a candidate as
non-Unicode, this function retries that call once with scrubbing
enabled.
Original candidate strings are restored on the way back so text
properties (Consult metadata, invisibility, faces) survive when a
scrubbed copy was passed to the module.  When full scores are
requested, score properties are attached after restoration so
scrubbed candidate identity is not lost when the module returns
copied strings."
  (let ((scrub-non-unicode
         (or nucleo-completion-scrub-non-unicode-candidates
             nucleo-completion--force-scrub-non-unicode-candidates)))
    (let ((bundle
           (condition-case err
               (nucleo-completion--module-results-with-scrub
                needle candidates ignore-case highlight-limit return-all-scores
                scrub-non-unicode history-ranks)
             (wrong-type-argument
              (if (and (not scrub-non-unicode)
                       (nucleo-completion--module-unicode-error-p err))
                  (nucleo-completion--module-results-with-scrub
                   needle candidates ignore-case highlight-limit
                   return-all-scores t history-ranks)
                (signal (car err) (cdr err)))))))
      (if return-all-scores
          (nucleo-completion--ensure-score-properties bundle)
        bundle))))

(defun nucleo-completion--bundle-candidates (bundle)
  "Return the candidate list from BUNDLE."
  (nth 0 bundle))

(defun nucleo-completion--bundle-interrupted-p (bundle)
  "Return non-nil when BUNDLE indicates an interrupted module call."
  (eq (nucleo-completion--bundle-candidates bundle)
      nucleo-completion--interrupted-sentinel))

(defun nucleo-completion--bundle-top-info (bundle)
  "Return the top-info list from BUNDLE."
  (nth 1 bundle))

(defun nucleo-completion--bundle-full-scores (bundle)
  "Return the parallel score list from BUNDLE, or nil."
  (nth 2 bundle))

(defun nucleo-completion--top-info-candidate (entry)
  "Return ENTRY's candidate."
  (nth 0 entry))

(defun nucleo-completion--top-info-score (entry)
  "Return ENTRY's score."
  (nth 1 entry))

(defun nucleo-completion--top-info-indices (entry)
  "Return ENTRY's precomputed highlight indices."
  (nth 2 entry))

(defun nucleo-completion--max-score (top-info full-scores)
  "Return the best score from TOP-INFO or FULL-SCORES."
  (or (and top-info
           (nucleo-completion--top-info-score (car top-info)))
      (car-safe full-scores)))

(defun nucleo-completion--top-info-hash (top-info)
  "Return a hash table mapping candidates in TOP-INFO to entries."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (entry top-info)
      (puthash (nucleo-completion--top-info-candidate entry) entry table))
    table))

(defun nucleo-completion--candidate-score (candidate)
  "Return the score text property from CANDIDATE, or nil."
  (and (stringp candidate)
       (get-text-property 0 nucleo-completion--score-property candidate)))

(defun nucleo-completion--propertize-score (candidate score)
  "Return a copy of CANDIDATE carrying SCORE as a text property."
  (let ((copy (copy-sequence candidate)))
    (put-text-property 0 (length copy)
                       nucleo-completion--score-property score copy)
    copy))

(defun nucleo-completion--propertize-score-candidates
    (candidates full-scores)
  "Return (CHANGED . CANDIDATES) after applying FULL-SCORES.
CANDIDATES are preserved when FULL-SCORES is shorter than the
candidate list, which keeps malformed module bundles from
silently dropping candidates."
  (let ((original candidates)
        (scores full-scores)
        changed
        result)
    (while candidates
      (let ((candidate (car candidates)))
        (push
         (if scores
             (let ((score (car scores)))
               (if (equal (nucleo-completion--candidate-score candidate)
                          score)
                   candidate
                 (setq changed t)
                 (nucleo-completion--propertize-score candidate score)))
           candidate)
         result))
      (setq candidates (cdr candidates)
            scores (cdr scores)))
    (when scores
      (setq changed t))
    (cons changed (if changed (nreverse result) original))))

(defun nucleo-completion--ensure-score-properties (bundle)
  "Ensure BUNDLE candidates carry score text properties.
The Rust module returns scores in BUNDLE's FULL-SCORES slot.  This
attaches missing score properties to candidates after any candidate
restoration, without building an `equal' hash table keyed by
candidate strings."
  (let ((candidates (nucleo-completion--bundle-candidates bundle))
        (full-scores (nucleo-completion--bundle-full-scores bundle)))
    (when full-scores
      (pcase-let ((`(,changed . ,candidates-with-scores)
                   (nucleo-completion--propertize-score-candidates
                    candidates full-scores)))
        (when changed
          (setcar bundle candidates-with-scores)))))
  bundle)

(defun nucleo-completion--history-rank-order
    (candidate-a candidate-b rank-table)
  "Return the history order between CANDIDATE-A and CANDIDATE-B.
The return value is `before' when CANDIDATE-A outranks
CANDIDATE-B in RANK-TABLE, `after' when CANDIDATE-B outranks
CANDIDATE-A, and nil when history does not order them."
  (let ((rank-a (gethash
                 (nucleo-completion--history-candidate-key candidate-a)
                 rank-table))
        (rank-b (gethash
                 (nucleo-completion--history-candidate-key candidate-b)
                 rank-table)))
    (cond
     ((and rank-a rank-b)
      (cond
       ((< rank-a rank-b) 'before)
       ((< rank-b rank-a) 'after)))
     (rank-a 'before)
     (rank-b 'after)
     (t nil))))

(defun nucleo-completion--history-rank-less-p
    (candidate-a candidate-b rank-table)
  "Return non-nil when CANDIDATE-A outranks CANDIDATE-B in RANK-TABLE."
  (eq (nucleo-completion--history-rank-order
       candidate-a candidate-b rank-table)
      'before))

(defun nucleo-completion--history-sort-before-p
    (candidate-a score-a position-a candidate-b score-b position-b rank-table)
  "Return non-nil when A should sort before B for history tie sorting.
CANDIDATE-A, SCORE-A, and POSITION-A describe A.  CANDIDATE-B,
SCORE-B, and POSITION-B describe B.  RANK-TABLE maps candidates
to history ranks."
  (let ((score-a (or score-a 0))
        (score-b (or score-b 0)))
    (if (/= score-a score-b)
        (> score-a score-b)
      (let ((history-order
             (nucleo-completion--history-rank-order
              candidate-a candidate-b rank-table)))
        (cond
         ((eq history-order 'before) t)
         ((eq history-order 'after) nil)
         (t (< position-a position-b)))))))

(defun nucleo-completion--top-info-adjacent-score-tie-p (top-info)
  "Return non-nil if sorted TOP-INFO has an adjacent score tie."
  (let ((previous (nucleo-completion--top-info-score (car top-info)))
        (top-info (cdr top-info))
        tied)
    (while (and top-info (not tied))
      (let ((score (nucleo-completion--top-info-score (car top-info))))
        (when (equal previous score)
          (setq tied t))
        (setq previous score
              top-info (cdr top-info))))
    tied))

(defun nucleo-completion--sort-top-info-ties-by-history
    (top-info rank-table)
  "Sort TOP-INFO by score and history rank using RANK-TABLE."
  (if (not (nucleo-completion--top-info-adjacent-score-tie-p top-info))
      top-info
    (mapcar
     #'car
     (sort
      (cl-loop for entry in top-info
               for position from 0
               collect (list entry position))
      (lambda (a b)
        (let ((entry-a (car a))
              (entry-b (car b)))
          (nucleo-completion--history-sort-before-p
           (nucleo-completion--top-info-candidate entry-a)
           (nucleo-completion--top-info-score entry-a)
           (cadr a)
           (nucleo-completion--top-info-candidate entry-b)
           (nucleo-completion--top-info-score entry-b)
           (cadr b)
           rank-table)))))))

(defun nucleo-completion--candidate-adjacent-score-tie-p
    (candidates scores)
  "Return non-nil when sorted CANDIDATES have an adjacent score tie.
SCORES are parallel to CANDIDATES.  When SCORES is shorter than
CANDIDATES, missing scores are treated as nil."
  (let ((previous (car scores))
        (candidates (cdr candidates))
        (scores (cdr scores))
        tied)
    (while (and candidates (not tied))
      (let ((score (car scores)))
        (when (equal previous score)
          (setq tied t))
        (setq previous score
              candidates (cdr candidates))
        (when scores
          (setq scores (cdr scores)))))
    tied))

(defun nucleo-completion--history-sort-items (candidates full-scores)
  "Return history sort items for CANDIDATES and FULL-SCORES.
The result preserves all CANDIDATES even when FULL-SCORES is
shorter than the candidate list."
  (let ((scores full-scores)
        (position 0)
        result)
    (dolist (candidate candidates (nreverse result))
      (push (list candidate (car scores) position) result)
      (when scores
        (setq scores (cdr scores)))
      (setq position (1+ position)))))

(defun nucleo-completion--sort-bundle-ties-by-history
    (bundle rank-table)
  "Sort equal-score candidates in BUNDLE by history using RANK-TABLE.
This is used for older Rust modules that do not provide the
history-aware batch API.  BUNDLE must include FULL-SCORES."
  (pcase-let ((`(,candidates ,top-info ,full-scores) bundle))
    (if (or (null candidates)
            (null full-scores)
            (not (nucleo-completion--candidate-adjacent-score-tie-p
                  candidates full-scores)))
        bundle
      (let ((items
             (sort
              (nucleo-completion--history-sort-items
               candidates full-scores)
              (lambda (a b)
                (nucleo-completion--history-sort-before-p
                 (nth 0 a) (nth 1 a) (nth 2 a)
                 (nth 0 b) (nth 1 b) (nth 2 b)
                 rank-table)))))
        (list (mapcar #'car items)
              (nucleo-completion--sort-top-info-ties-by-history
               top-info rank-table)
              (mapcar #'cadr items))))))

(defun nucleo-completion--ascii-string-p (string)
  "Return non-nil if every character in STRING is ASCII."
  (let ((index 0)
        (length (length string))
        (ascii t))
    (while (and ascii (< index length))
      (when (> (aref string index) #x7f)
        (setq ascii nil))
      (setq index (1+ index)))
    ascii))

(defun nucleo-completion--promote-regexp-only-candidate-p (candidate)
  "Return non-nil when regexp-only CANDIDATE should precede module matches."
  (pcase nucleo-completion-regexp-only-match-priority
    ('before t)
    ('after nil)
    ('non-ascii (not (nucleo-completion--ascii-string-p candidate)))
    (_ (not (nucleo-completion--ascii-string-p candidate)))))

(defun nucleo-completion--split-regexp-only-candidates (candidates)
  "Split regexp-only CANDIDATES into before- and after-module lists.
Return (BEFORE . AFTER)."
  (pcase nucleo-completion-regexp-only-match-priority
    ('before (cons candidates nil))
    ('after (cons nil candidates))
    (_
     (let (before after)
       (dolist (candidate candidates)
         (if (nucleo-completion--promote-regexp-only-candidate-p candidate)
             (push candidate before)
           (push candidate after)))
       (cons (nreverse before) (nreverse after))))))

(defun nucleo-completion--at-least-as-many-candidates-p
    (candidates reference)
  "Return non-nil when CANDIDATES has at least as many items as REFERENCE."
  (let ((candidates candidates)
        (reference reference))
    (while (and candidates reference)
      (setq candidates (cdr candidates)
            reference (cdr reference)))
    (null reference)))

(defun nucleo-completion--regexp-only-matching-candidates
    (needle scorable)
  "Return SCORABLE candidates that match NEEDLE's regexp-only pass."
  (let ((case-fold-search completion-ignore-case)
        (regexp-groups (nucleo-completion--regexp-only-regexp-groups needle))
        result)
    (dolist (candidate scorable (nreverse result))
      (when (nucleo-completion--regexp-match-p regexp-groups candidate)
        (push candidate result)))))

(defun nucleo-completion--regexp-only-candidates
    (needle scorable module-candidates)
  "Return SCORABLE candidates not in MODULE-CANDIDATES that match NEEDLE regexps.
Walk SCORABLE once."
  (unless (nucleo-completion--at-least-as-many-candidates-p
           module-candidates scorable)
    (if (null module-candidates)
        (nucleo-completion--regexp-only-matching-candidates
         needle scorable)
      (let ((seen (make-hash-table :test #'equal))
            (case-fold-search completion-ignore-case)
            (regexp-groups (nucleo-completion--regexp-only-regexp-groups
                            needle))
            result)
        (dolist (candidate module-candidates)
          (puthash candidate t seen))
        (dolist (candidate scorable)
          (unless (gethash candidate seen)
            (when (nucleo-completion--regexp-match-p regexp-groups candidate)
              (push candidate result))))
        (nreverse result)))))

(defun nucleo-completion--regexp-only-top-info (candidates score)
  "Return synthetic top-info entries for regexp-only CANDIDATES with SCORE."
  (mapcar (lambda (candidate) (list candidate score nil)) candidates))

(defun nucleo-completion--regexp-only-score-candidates
    (candidates score propertize-p)
  "Return (CANDIDATES . SCORES) for regexp-only CANDIDATES.
Every entry in SCORES is SCORE.  When PROPERTIZE-P is non-nil,
return copied candidates carrying SCORE as a text property;
otherwise preserve the original CANDIDATES list."
  (let (scores)
    (if propertize-p
        (let (propertized)
          (dolist (candidate candidates)
            (push (nucleo-completion--propertize-score candidate score)
                  propertized)
            (push score scores))
          (cons (nreverse propertized) (nreverse scores)))
      (dolist (_candidate candidates)
        (push score scores))
      (cons candidates (nreverse scores)))))

(defun nucleo-completion--merge-regexp-only-matches
    (needle scorable bundle include-top-info-p)
  "Return BUNDLE merged with regexp-only matches from SCORABLE.
NEEDLE is the current completion pattern.
BUNDLE is (CANDIDATES TOP-INFO FULL-SCORES).  Return a new
bundle with the same shape.  When INCLUDE-TOP-INFO-P is non-nil,
each regexp-only candidate gets a synthetic top-info entry (CAND
MAX-SCORE nil) so the score-band highlighter still classifies
these matches in the high band.  FULL-SCORES, when present, is
extended with MAX-SCORE entries to keep the parallel-array
invariant.  MAX-SCORE is only computed when either extension is
needed.

Candidates are partitioned according to
`nucleo-completion-regexp-only-match-priority'."
  (pcase-let* ((`(,module-candidates ,top-info ,full-scores) bundle)
               (extra (nucleo-completion--regexp-only-candidates
                       needle scorable module-candidates)))
    (if (null extra)
        bundle
      (let* ((max-score (when (or full-scores include-top-info-p)
                          (or (nucleo-completion--max-score
                               top-info full-scores)
                              1)))
             (split (nucleo-completion--split-regexp-only-candidates extra))
             (before (car split))
             (after (cdr split))
             before-scores
             after-scores)
        (when full-scores
          (pcase-let ((`(,scored-before . ,scores-before)
                       (nucleo-completion--regexp-only-score-candidates
                        before max-score
                        nucleo-completion-highlight-score-bands))
                      (`(,scored-after . ,scores-after)
                       (nucleo-completion--regexp-only-score-candidates
                        after max-score
                        nucleo-completion-highlight-score-bands)))
            (setq before scored-before
                  after scored-after
                  before-scores scores-before
                  after-scores scores-after)))
        (list (append before module-candidates after)
              (if include-top-info-p
                  (append
                   (nucleo-completion--regexp-only-top-info before max-score)
                   top-info
                   (nucleo-completion--regexp-only-top-info after max-score))
                top-info)
              (when full-scores
                (append before-scores full-scores after-scores)))))))

(defun nucleo-completion--exact-word-regexps (needle)
  "Return regexps that match NEEDLE terms as complete words."
  (nucleo-completion--cached
   nucleo-completion--exact-word-regexp-cache
   needle
   (lambda () (nucleo-completion--exact-word-regexps-1 needle))))

(defun nucleo-completion--exact-word-regexps-1 (needle)
  "Return uncached regexps that match NEEDLE terms as complete words."
  (mapcar (lambda (term)
            (concat "\\(?:\\`\\|[^[:alnum:]_]\\)"
                    (regexp-quote term)
                    "\\(?:\\'\\|[^[:alnum:]_]\\)"))
          (nucleo-completion--terms needle)))

(defun nucleo-completion--exact-word-match-p (needle candidate)
  "Return non-nil when every NEEDLE term occurs as a word in CANDIDATE."
  (let ((case-fold-search completion-ignore-case))
    (cl-every (lambda (regexp)
                (string-match-p regexp candidate))
              (nucleo-completion--exact-word-regexps needle))))

(defun nucleo-completion--high-score-p
    (needle candidate score max-score)
  "Return non-nil if CANDIDATE belongs to NEEDLE's high score band.
SCORE is compared to MAX-SCORE."
  (or (nucleo-completion--numeric-high-score-p score max-score)
      (nucleo-completion--exact-word-match-p needle candidate)))

(defun nucleo-completion--numeric-high-score-p (score max-score)
  "Return non-nil when SCORE is high enough relative to MAX-SCORE."
  (and (numberp score)
       (numberp max-score)
       (> max-score 0)
       (>= score (* max-score
                    (nucleo-completion--high-score-ratio)))))

(defun nucleo-completion--score-band-needs-exact-cache-p (score max-score)
  "Return non-nil when SCORE may need exact-word fallback under MAX-SCORE."
  (and nucleo-completion-highlight-score-bands
       score
       (not (nucleo-completion--numeric-high-score-p score max-score))))

(defun nucleo-completion--high-score-ratio ()
  "Return `nucleo-completion-high-score-ratio' clamped to [0, 1]."
  (if (numberp nucleo-completion-high-score-ratio)
      (min 1.0 (max 0.0 nucleo-completion-high-score-ratio))
    0.85))

(defun nucleo-completion--score-band-face
    (needle candidate score max-score)
  "Return the score-band face for CANDIDATE matching NEEDLE.
SCORE is compared to MAX-SCORE."
  (if (nucleo-completion--high-score-p needle candidate score max-score)
      (append '(nucleo-completion-high-score-face)
              nucleo-completion-high-score-emphasis)
    'nucleo-completion-low-score-face))

(defun nucleo-completion--apply-score-band
    (needle haystack score max-score)
  "Apply a score-band background to HAYSTACK for NEEDLE.
SCORE is compared to MAX-SCORE."
  (when (and nucleo-completion-highlight-score-bands score)
    (add-face-text-property
     0 (length haystack)
     (nucleo-completion--score-band-face needle haystack score max-score)
     t haystack))
  haystack)

(defun nucleo-completion--highlight-regexp (regexp haystack)
  "Highlight REGEXP matches in HAYSTACK."
  (let (start)
    (while (let ((position (or start 0)))
             (and (<= position (length haystack))
                  (string-match regexp haystack position)))
      (let ((beg (match-beginning 0))
            (end (match-end 0)))
        (if (and (integerp beg) (integerp end) (< beg end))
            (progn
              (add-face-text-property beg end
                                      'completions-common-part nil haystack)
              (setq start end))
          (setq start (1+ (or start 0))))))))

(defun nucleo-completion-highlight (needle haystack &optional indices)
  "Highlight destructively the NEEDLE matches in HAYSTACK.
When INDICES is non-nil, highlight those precomputed match
positions instead of recomputing a subsequence match."
  (let* ((case-fold-search completion-ignore-case)
         (terms (and (or (not indices) nucleo-completion-regexp-functions)
                     (nucleo-completion--terms needle))))
    (if indices
        (dolist (index indices)
          (when (and (integerp index)
                     (<= 0 index)
                     (< index (length haystack)))
            (add-face-text-property index (1+ index)
                                    'completions-common-part nil haystack)))
      (dolist (term terms)
        (let ((start 0))
          (cl-loop for ch across term
                   for match = (string-match (regexp-quote (char-to-string ch))
                                             haystack start)
                   while match
                   do (add-face-text-property match (1+ match)
                                              'completions-common-part nil haystack)
                   (setq start (1+ match))))))
    (when nucleo-completion-regexp-functions
      (dolist (term terms)
        (dolist (regexp (nucleo-completion--regexp-function-regexps term))
          (nucleo-completion--highlight-regexp regexp haystack)))))
  haystack)

(defun nucleo-completion--highlight-candidate
    (needle haystack &optional score max-score indices)
  "Highlight HAYSTACK against NEEDLE with match and optional score-band faces.
SCORE is compared to MAX-SCORE when both are non-nil."
  (setq haystack (nucleo-completion--apply-score-band
                  needle haystack score max-score))
  (nucleo-completion-highlight needle haystack indices))

(defun nucleo-completion--completion-regexp-list
    (needle expanded-regexp-p module-p)
  "Return `completion-regexp-list' adjusted for NEEDLE.
EXPANDED-REGEXP-P and MODULE-P skip adding fallback regexps when
another filtering path already handles regexp expansion."
  (if (or expanded-regexp-p module-p)
      completion-regexp-list
    (append
     (if nucleo-completion-regexp-functions
         (apply #'append (nucleo-completion--term-regexp-groups needle))
       (mapcar #'nucleo-completion--term-fuzzy-regexp
               (nucleo-completion--terms needle)))
     completion-regexp-list)))

(defun nucleo-completion--generated-regexp-list-only-p
    (module-p expanded-regexp-p external-regexp-list)
  "Return non-nil when only Nucleo-generated regexps would be added.
MODULE-P, EXPANDED-REGEXP-P, and EXTERNAL-REGEXP-LIST describe
the current filtering path."
  (and (not module-p)
       (not expanded-regexp-p)
       (null external-regexp-list)))

(defun nucleo-completion--initial-regexp-list-data
    (prefix needle table pred module-p expanded-regexp-p external-regexp-list)
  "Return (REGEXP-LIST . GENERATED-REGEXP-LIST-P) for initial completion.
PREFIX, NEEDLE, TABLE, PRED, MODULE-P, EXPANDED-REGEXP-P, and
EXTERNAL-REGEXP-LIST describe the current completion request.
When a plain list TABLE can skip `all-completions', Nucleo's
generated regexp list is unused because the fallback filter
applies the same conditions directly.  In that case avoid
building the regexp list."
  (let ((generated-regexp-list-p
         (nucleo-completion--generated-regexp-list-only-p
          module-p expanded-regexp-p external-regexp-list)))
    (cons
     (if (and generated-regexp-list-p
              (nucleo-completion--initial-completion-list-fast-path-p
               prefix needle table pred nil t))
         external-regexp-list
       (nucleo-completion--completion-regexp-list
        needle expanded-regexp-p module-p))
     generated-regexp-list-p)))

(defun nucleo-completion--initial-completion-list-fast-path-p
    (prefix needle table pred regexp-list
            &optional generated-regexp-list-p)
  "Return non-nil when TABLE can be used without `all-completions'.
PREFIX, NEEDLE, PRED, and REGEXP-LIST describe the current
completion request.
GENERATED-REGEXP-LIST-P means REGEXP-LIST only contains regexps
that Nucleo will apply during its own fallback filtering."
  (and (string= prefix "") (stringp (car-safe table))
       (not pred)
       (not (string= needle ""))
       (or generated-regexp-list-p
           (null regexp-list))))

(defun nucleo-completion--initial-completion-candidates
    (prefix needle table pred regexp-list)
  "Return completion candidates before Nucleo filtering.
PREFIX, NEEDLE, TABLE, and PRED have the same meaning as in
`nucleo-completion--all-completions-1'.  REGEXP-LIST is the
currently effective `completion-regexp-list'."
  (if (nucleo-completion--initial-completion-list-fast-path-p
       prefix needle table pred regexp-list)
      table
    (let ((completion-regexp-list regexp-list))
      (all-completions prefix table pred))))

(defun nucleo-completion--initial-completion-data
    (prefix needle table pred regexp-list &optional generated-regexp-list-p)
  "Return (CANDIDATES . BASE-SIZE) before Nucleo filtering.
PREFIX, NEEDLE, TABLE, PRED, and REGEXP-LIST describe the current
completion request.
Plain list tables cannot carry an `all-completions' base-size
tail marker, so the fast path avoids an extra full-list scan.
GENERATED-REGEXP-LIST-P means REGEXP-LIST only contains regexps
that Nucleo will apply during its own fallback filtering."
  (if (nucleo-completion--initial-completion-list-fast-path-p
       prefix needle table pred regexp-list generated-regexp-list-p)
      (cons table nil)
    (nucleo-completion--split-base-size
     (nucleo-completion--initial-completion-candidates
      prefix needle table pred regexp-list))))

(defun nucleo-completion--completion-list-base-size (candidates)
  "Return CANDIDATES' base-size marker, or nil."
  (when (consp candidates)
    (let ((tail candidates))
      (while (consp (cdr tail))
        (setq tail (cdr tail)))
      (and (integerp (cdr tail))
           (cdr tail)))))

(defun nucleo-completion--copy-without-base-size (candidates)
  "Return a proper-list copy of CANDIDATES without its base-size marker."
  (let (head tail)
    (while (consp candidates)
      (let ((cell (list (car candidates))))
        (if head
            (setcdr tail cell)
          (setq head cell))
        (setq tail cell
              candidates (cdr candidates))))
    head))

(defun nucleo-completion--split-base-size (candidates)
  "Return (COMPLETIONS . BASE-SIZE) for CANDIDATES.
COMPLETIONS is a proper list suitable for filtering.  BASE-SIZE
is the integer marker from CANDIDATES' last cdr, or nil."
  (let ((base-size
         (nucleo-completion--completion-list-base-size candidates)))
    (cons (if base-size
              (nucleo-completion--copy-without-base-size candidates)
            candidates)
          base-size)))

(defun nucleo-completion--active-history-variable-p ()
  "Return non-nil when minibuffer history tie sorting has usable history state."
  (and nucleo-completion-sort-ties-by-history
       (minibufferp)
       (boundp 'minibuffer-history-variable)
       (symbolp minibuffer-history-variable)
       (not (eq minibuffer-history-variable t))
       (boundp minibuffer-history-variable)))

(defun nucleo-completion--history-candidate-key (candidate)
  "Return CANDIDATE's string key for history lookup."
  (when (stringp candidate)
    candidate))

(defun nucleo-completion--history-default ()
  "Return the active minibuffer default as a string, or nil."
  (cond
   ((not (boundp 'minibuffer-default)) nil)
   ((stringp minibuffer-default) minibuffer-default)
   ((stringp (car-safe minibuffer-default)) (car minibuffer-default))))

(defun nucleo-completion--history-entry-candidate (prefix entry)
  "Return history ENTRY as a candidate relative to PREFIX, or nil."
  (when (stringp entry)
    (if (string-empty-p prefix)
        entry
      (when (string-prefix-p prefix entry)
        (substring entry (length prefix))))))

(defun nucleo-completion--history-rank-table-from-entries (entries &optional prefix)
  "Return a hash table mapping history ENTRIES under PREFIX to ranks."
  (let ((rank 0)
        (prefix (or prefix ""))
        table)
    (dolist (entry entries)
      (let* ((entry (nucleo-completion--history-candidate-key entry))
             (candidate (and entry
                             (nucleo-completion--history-entry-candidate
                              prefix entry))))
        (when candidate
          (unless table
            (setq table (make-hash-table :test #'equal)))
          (when (eq (gethash candidate table :missing) :missing)
            (puthash candidate rank table)
            (setq rank (1+ rank))))))
    (when (> rank 0)
      table)))

(defun nucleo-completion--minibuffer-history-rank-table (prefix)
  "Return a hash table mapping minibuffer history candidates under PREFIX."
  (when (nucleo-completion--active-history-variable-p)
    (let* ((history (symbol-value minibuffer-history-variable))
           (entries (if (listp history) history nil))
           (default (nucleo-completion--history-default)))
      (nucleo-completion--history-rank-table-from-entries
       (if default (cons default entries) entries)
       prefix))))

(defun nucleo-completion--corfu-history-rank-table ()
  "Return a hash table mapping Corfu history candidates to ranks."
  (when (and nucleo-completion-sort-ties-by-history
             (not (minibufferp))
             (bound-and-true-p corfu-history-mode)
             (boundp 'corfu-history)
             (listp corfu-history))
    (nucleo-completion--history-rank-table-from-entries corfu-history)))

(defun nucleo-completion--history-rank-table (prefix)
  "Return a hash table mapping available history candidates to ranks.
PREFIX is used to interpret minibuffer history entries.  Ordinary
buffer completion uses `corfu-history' without prefix stripping
when `corfu-history-mode' is enabled."
  (or (nucleo-completion--minibuffer-history-rank-table prefix)
      (nucleo-completion--corfu-history-rank-table)))

(defun nucleo-completion--history-ranking (prefix candidates)
  "Return (RANKS . TABLE) for CANDIDATES under PREFIX, or nil.
RANKS is parallel to CANDIDATES and contains integers for
history entries and nil for candidates not present in history.
TABLE maps candidate strings to their history ranks."
  (when (cdr candidates)
    (let ((table (nucleo-completion--history-rank-table prefix)))
      (when table
        (let (ranks any)
          (dolist (candidate candidates)
            (let ((rank (gethash
                         (nucleo-completion--history-candidate-key candidate)
                         table)))
              (when rank
                (setq any t))
              (push rank ranks)))
          (when any
            (cons (nreverse ranks) table)))))))

(defun nucleo-completion--module-completion-results
    (prefix needle all expanded-regexp-p highlight-limit need-full-scores)
  "Return module-backed filtering result for NEEDLE and ALL.
PREFIX is used to interpret minibuffer history entries.  When
EXPANDED-REGEXP-P is non-nil, merge regexp-only matches.
HIGHLIGHT-LIMIT and NEED-FULL-SCORES are passed to the module.
The return value has the shape (ALL BUNDLE TOP-INFO FULL-SCORES)."
  (let* ((history-ranking (nucleo-completion--history-ranking prefix all))
         (history-ranks (car-safe history-ranking))
         (history-rank-table (cdr-safe history-ranking))
         (module-history-p (and history-ranks
                                (nucleo-completion--module-supports-history-p)))
         (need-full-scores
          (or need-full-scores
              (and history-ranks (not module-history-p))))
         (bundle (nucleo-completion--module-results
                  needle all completion-ignore-case
                  highlight-limit need-full-scores
                  (and module-history-p history-ranks)))
         top-info
         full-scores)
    (when (nucleo-completion--bundle-interrupted-p bundle)
      (throw 'nucleo-completion-interrupted t))
    (when (and history-rank-table (not module-history-p))
      (setq bundle
            (nucleo-completion--sort-bundle-ties-by-history
             bundle history-rank-table)))
    (when expanded-regexp-p
      (setq bundle
            (nucleo-completion--merge-regexp-only-matches
             needle all bundle
             (and (> highlight-limit 0)
                  nucleo-completion-highlight-score-bands
                  (not need-full-scores)))))
    (setq top-info (nucleo-completion--bundle-top-info bundle)
          full-scores (nucleo-completion--bundle-full-scores bundle))
    (list (nucleo-completion--bundle-candidates bundle)
          bundle top-info full-scores)))

(defun nucleo-completion--filter-completions
    (prefix needle all module-p expanded-regexp-p
            highlight-limit need-full-scores)
  "Return filtered completion data for NEEDLE and ALL.
PREFIX is used to interpret minibuffer history entries.  MODULE-P
selects the Rust module path.  EXPANDED-REGEXP-P, HIGHLIGHT-LIMIT,
and NEED-FULL-SCORES are passed through when the module path is used.
The return value has the shape (ALL BUNDLE TOP-INFO FULL-SCORES)."
  (cond
   ((or (null all) (string= needle ""))
    (list all nil nil nil))
   (module-p
    (nucleo-completion--module-completion-results
     prefix needle all expanded-regexp-p highlight-limit need-full-scores))
   (t
    (list (nucleo-completion--fallback-filter needle all prefix)
          nil nil nil))))

(defun nucleo-completion--record-current-result
    (prefix needle all &optional base-size)
  "Record ALL as the current completion result for PREFIX and NEEDLE.
BASE-SIZE is the completion table's base-size marker before
PREFIX is applied."
  (let ((prefix (if (stringp prefix) prefix "")))
    (setq nucleo-completion--filtering-p (not (string= needle "")))
    (setq nucleo-completion--current-prefix prefix
          nucleo-completion--current-base-size base-size
          nucleo-completion--current-result
          (copy-sequence all))))

(defun nucleo-completion--clear-completion-state (prefix filtering-p)
  "Clear reusable completion state for PREFIX.
FILTERING-P becomes the new value of
`nucleo-completion--filtering-p'."
  (setq nucleo-completion--filtering-p filtering-p
        nucleo-completion--current-prefix (if (stringp prefix) prefix "")
        nucleo-completion--current-base-size nil
        nucleo-completion--current-result nil)
  (when (boundp 'completion-lazy-hilit-fn)
    (setq completion-lazy-hilit-fn nil)))

(defun nucleo-completion--reset-completion-state ()
  "Clear state from a Nucleo completion attempt with no result."
  (nucleo-completion--clear-completion-state "" nil))

(defun nucleo-completion--install-lazy-highlight
    (needle top-info max-score)
  "Install lazy highlighting for NEEDLE.
TOP-INFO provides score and index lookup data.  MAX-SCORE is used
for score-band classification."
  (let ((info-hash (and top-info
                        (nucleo-completion--top-info-hash top-info)))
        exact-word-regexp-cache)
    (setq completion-lazy-hilit-fn
          (lambda (candidate)
            (let* ((info (and info-hash
                              (gethash candidate info-hash)))
                   (score (or (and info
                                   (nucleo-completion--top-info-score
                                    info))
                              (nucleo-completion--candidate-score
                               candidate)))
                   (indices (and info
                                 (nucleo-completion--top-info-indices
                                  info))))
              (when (and (nucleo-completion--score-band-needs-exact-cache-p
                          score max-score)
                         (not exact-word-regexp-cache))
                (setq exact-word-regexp-cache
                      (make-hash-table :test #'equal)))
              (let ((nucleo-completion--exact-word-regexp-cache
                     exact-word-regexp-cache))
                (nucleo-completion--highlight-candidate
                 needle (copy-sequence candidate) score max-score indices)))))))

(defun nucleo-completion--highlight-eager
    (needle all top-info max-score highlight-limit)
  "Eagerly highlight up to HIGHLIGHT-LIMIT candidates in ALL.
NEEDLE is the current completion pattern.  TOP-INFO and MAX-SCORE
provide score and index data when available."
  (let ((info-hash (and top-info
                        (nucleo-completion--top-info-hash top-info)))
        (cell all)
        (remaining highlight-limit))
    (while (and (> remaining 0) cell)
      (let* ((candidate (car cell))
             (info (and info-hash (gethash candidate info-hash)))
             (score (and info
                         (nucleo-completion--top-info-score info)))
             (indices (and info
                           (nucleo-completion--top-info-indices info))))
        (unless score
          (setq score (nucleo-completion--candidate-score candidate)))
        (when (and (nucleo-completion--score-band-needs-exact-cache-p
                    score max-score)
                   (not nucleo-completion--exact-word-regexp-cache))
          (setq nucleo-completion--exact-word-regexp-cache
                (make-hash-table :test #'equal)))
        (setcar cell
                (nucleo-completion--highlight-candidate
                 needle (copy-sequence candidate) score max-score indices)))
      (setq cell (cdr cell)
            remaining (1- remaining))))
  all)

(defun nucleo-completion--highlight-completions
    (needle all top-info max-score highlight-limit lazy-hilit-p)
  "Apply lazy or eager highlighting setup to ALL.
NEEDLE is the current completion pattern.  TOP-INFO, MAX-SCORE,
and HIGHLIGHT-LIMIT provide module metadata.  LAZY-HILIT-P
selects lazy highlighting."
  (cond
   (lazy-hilit-p
    (nucleo-completion--install-lazy-highlight
     needle top-info max-score)
    all)
   ((and (> highlight-limit 0) all)
    (nucleo-completion--highlight-eager
     needle all top-info max-score highlight-limit))
   (t all)))

(defun nucleo-completion--with-base-size (prefix all &optional base-size)
  "Return ALL with PREFIX and optional BASE-SIZE marker applied."
  (let* ((prefix-size (length prefix))
         (base-size (cond
                     (base-size (+ prefix-size base-size))
                     ((> prefix-size 0) prefix-size))))
    (if (and all base-size)
        (nconc all base-size)
      all)))

(defun nucleo-completion--native-try-result
    (string point table pred &optional allow-point-only)
  "Return TABLE's native `try-completion' refinement for STRING.
POINT is the current point in STRING.  This lets table wrappers
such as `completion-table-with-terminator' add their own final
text while keeping Nucleo responsible for choosing the candidate
set.  When ALLOW-POINT-ONLY is non-nil, return a refinement that
only moves point after an earlier Nucleo completion changed the
text."
  (let* ((beforepoint (substring string 0 point))
         (afterpoint (substring string point))
         (completion (try-completion beforepoint table pred)))
    (when (stringp completion)
      (let* ((suffix (if (fboundp 'completion--merge-suffix)
                         (completion--merge-suffix completion point afterpoint)
                       afterpoint))
             (completed (concat completion suffix))
             (newpoint (length completion)))
        (when (or (not (string= completed string))
                  (and allow-point-only (/= newpoint point)))
          (cons completed newpoint))))))

(defun nucleo-completion--refine-try-result
    (result original-string original-point table pred)
  "Refine RESULT with native `try-completion' for TABLE.
ORIGINAL-STRING and ORIGINAL-POINT describe the user input before
Nucleo filtering.  TABLE and PRED are passed to the native
completion table.  RESULT follows `completion-try-completion'
semantics."
  (let (string point)
    (cond
     ((eq result t)
      (setq string original-string
            point original-point))
     ((and (consp result)
           (stringp (car result))
           (integerp (cdr result))
           (or (not (string= (car result) original-string))
               (/= (cdr result) original-point)))
      (setq string (car result)
            point (cdr result))))
    (if string
        (or (nucleo-completion--native-try-result
             string point table pred
             (and (consp result)
                  (not (string= (car result) original-string))))
            result)
      result)))

(defun nucleo-completion--field-state (string table pred point)
  "Return completion field data for STRING at POINT.
TABLE and PRED identify the completion table and predicate.
The result has the form (POINT PREFIX BEFORE FIELD-SUFFIX
EXTERNAL-SUFFIX PATTERN).  BEFORE is the completion field text
before point.  FIELD-SUFFIX is the text after point that still
belongs to the same completion boundary.  EXTERNAL-SUFFIX is text
after the completion boundary.  PATTERN is BEFORE and
FIELD-SUFFIX concatenated for filtering and highlighting."
  (let* ((point (or point (length string)))
         (beforepoint (substring string 0 point))
         (afterpoint (substring string point))
         (bounds (completion-boundaries beforepoint table pred afterpoint))
         (prefix (substring beforepoint 0 (car bounds)))
         (before (substring beforepoint (car bounds)))
         (field-suffix (substring afterpoint 0 (cdr bounds)))
         (external-suffix (substring afterpoint (cdr bounds))))
    (list point prefix before field-suffix external-suffix
          (concat before field-suffix))))

(defun nucleo-completion--plain-unique-candidates (candidates)
  "Return CANDIDATES without text properties or duplicate strings."
  (cond
   ((null candidates) nil)
   ((null (cdr candidates))
    (list (nucleo-completion--plain-candidate (car candidates))))
   ((null (cddr candidates))
    (let ((first (nucleo-completion--plain-candidate (car candidates)))
          (second (nucleo-completion--plain-candidate (cadr candidates))))
      (if (equal first second)
          (list first)
        (list first second))))
   (t
    (let ((seen (make-hash-table :test #'equal))
          result)
      (dolist (candidate candidates)
        (let ((plain (nucleo-completion--plain-candidate candidate)))
          (unless (gethash plain seen)
            (puthash plain t seen)
            (push plain result))))
      (nreverse result)))))

(defun nucleo-completion--string-properties-p (string)
  "Return non-nil when STRING has text properties."
  (or (text-properties-at 0 string)
      (next-property-change 0 string)))

(defun nucleo-completion--plain-candidate (candidate)
  "Return CANDIDATE without text properties.
Plain strings are returned unchanged to avoid copying hot-path
`try-completion' candidates."
  (if (nucleo-completion--string-properties-p candidate)
      (substring-no-properties candidate)
    candidate))

(defun nucleo-completion--try-result
    (string point prefix pattern suffix candidates)
  "Return a `try-completion' result from filtered CANDIDATES.
STRING and POINT are the original completion input.  PREFIX and
SUFFIX are outside the completion field.  PATTERN is the full
completion field text used for filtering.  When multiple
candidates exist but their common prefix does not extend PATTERN,
return STRING unchanged at POINT to indicate that matches exist."
  (let ((plain-candidates
         (nucleo-completion--plain-unique-candidates candidates)))
    (cond
     ((null plain-candidates) nil)
     ((null (cdr plain-candidates))
      (let* ((candidate (car plain-candidates))
             (completion (concat prefix candidate suffix))
             (newpoint (+ (length prefix) (length candidate))))
        (if (string= completion string)
            t
          (cons completion newpoint))))
     (t
      (let ((common (try-completion "" plain-candidates)))
        (if (and (stringp common)
                 (> (length common) (length pattern)))
            (cons (concat prefix common suffix)
                  (+ (length prefix) (length common)))
          (cons string point)))))))

(defun nucleo-completion--try-completion-with-filter
    (string table pred point &optional field-state)
  "Try completing STRING by using Nucleo's filtered candidate set.
TABLE is the completion table.  PRED and POINT follow
`completion-try-completion' semantics.  FIELD-STATE, when non-nil,
is the precomputed result of `nucleo-completion--field-state'."
  (pcase-let* ((`(,point ,prefix ,_before ,_field-suffix ,suffix ,pattern)
                (or field-state
                    (nucleo-completion--field-state string table pred point)))
               (module-p (nucleo-completion--module-ready-p))
               (expanded-regexp-p
                (nucleo-completion--expanded-regexp-p pattern))
               (external-completion-regexp-list completion-regexp-list)
               (initial-regexp-list-data
                (nucleo-completion--initial-regexp-list-data
                 prefix pattern table pred module-p expanded-regexp-p
                 external-completion-regexp-list))
               (completion-regexp-list (car initial-regexp-list-data))
               (generated-regexp-list-p (cdr initial-regexp-list-data))
               (initial-data (nucleo-completion--initial-completion-data
                              prefix pattern table pred completion-regexp-list
                              generated-regexp-list-p))
               (all (car initial-data))
               (base-size (cdr initial-data)))
    (pcase-let ((`(,all ,_bundle ,_top-info ,_full-scores)
                 (nucleo-completion--filter-completions
                  prefix pattern all module-p expanded-regexp-p 0 nil)))
      (if all
          (progn
            (nucleo-completion--record-current-result
             prefix pattern all base-size)
            (nucleo-completion--refine-try-result
             (nucleo-completion--try-result
              string point prefix pattern suffix all)
             string point table pred))
        (nucleo-completion--reset-completion-state)
        nil))))

(defun nucleo-completion--complex-try-p (pattern expanded-regexp-p)
  "Return non-nil when flex `try-completion' cannot model PATTERN.
EXPANDED-REGEXP-P is non-nil when regexp expanders produced extra
matchers for PATTERN."
  (or expanded-regexp-p
      (cdr (nucleo-completion--terms pattern))))

(defun nucleo-completion--try-completion-1 (string table pred point)
  "Implement `nucleo-completion-try-completion' for STRING.
TABLE, PRED, and POINT follow `completion-try-completion'
semantics."
  (let ((field-state
         (nucleo-completion--field-state string table pred point)))
    (pcase-let ((`(,point ,prefix ,_before ,_field-suffix ,_suffix ,pattern)
                 field-state))
      (cond
       ((nucleo-completion--flex-nospace-p pattern)
        (nucleo-completion--reset-completion-state))
       ((string= pattern "")
        (nucleo-completion--reset-completion-state)
        (nucleo-completion--refine-try-result
         (completion-flex-try-completion string table pred point)
         string point table pred))
       (t
        (let* ((nucleo-completion--terms-cache
                (make-hash-table :test #'equal))
               (nucleo-completion--subsequence-regexp-cache
                (make-hash-table :test #'equal))
               (nucleo-completion--regexp-cache
                (and nucleo-completion-regexp-functions
                     (make-hash-table :test #'equal)))
               (expanded-regexp-p
                (nucleo-completion--expanded-regexp-p pattern)))
          (if (nucleo-completion--complex-try-p pattern expanded-regexp-p)
              (nucleo-completion--try-completion-with-filter
               string table pred point field-state)
            (let ((result (completion-flex-try-completion
                           string table pred point)))
              (if result
                  (progn
                    (nucleo-completion--clear-completion-state prefix t)
                    (nucleo-completion--refine-try-result
                     result string point table pred))
                (nucleo-completion--try-completion-with-filter
                 string table pred point field-state))))))))))

;;;###autoload
(defun nucleo-completion-try-completion (string table pred point)
  "Try completing STRING in TABLE with the Nucleo completion style.
PRED and POINT follow `completion-try-completion' semantics."
  (nucleo-completion--try-completion-1 string table pred point))

(defun nucleo-completion--all-completions-1 (string table &optional pred point)
  "Get Nucleo completions of STRING in TABLE.
See `completion-all-completions' for the semantics of PRED and POINT."
  (let ((nucleo-completion--exact-word-regexp-cache nil))
    (pcase-let ((`(,_point ,prefix ,_before ,_field-suffix ,_suffix ,pattern)
                 (nucleo-completion--field-state string table pred point)))
      (if (nucleo-completion--flex-nospace-p pattern)
          (nucleo-completion--reset-completion-state)
        (if (string= pattern "")
            (progn
              (nucleo-completion--reset-completion-state)
              (pcase-let ((`(,all . ,base-size)
                           (nucleo-completion--split-base-size
                            (nucleo-completion--initial-completion-candidates
                             prefix pattern table pred completion-regexp-list))))
                (nucleo-completion--with-base-size prefix all base-size)))
          (let* ((nucleo-completion--terms-cache
                  (make-hash-table :test #'equal))
                 (nucleo-completion--subsequence-regexp-cache
                  (make-hash-table :test #'equal))
                 (nucleo-completion--regexp-cache
                  (and nucleo-completion-regexp-functions
                       (make-hash-table :test #'equal)))
                 (module-p (nucleo-completion--module-ready-p))
                 (expanded-regexp-p
                  (nucleo-completion--expanded-regexp-p pattern))
                 (highlight-limit (nucleo-completion--highlight-limit))
                 (lazy-hilit-p (bound-and-true-p completion-lazy-hilit))
                 (need-full-scores
                  (and lazy-hilit-p nucleo-completion-highlight-score-bands))
                 (external-completion-regexp-list completion-regexp-list)
                 (initial-regexp-list-data
                  (nucleo-completion--initial-regexp-list-data
                   prefix pattern table pred module-p expanded-regexp-p
                   external-completion-regexp-list))
                 (completion-regexp-list (car initial-regexp-list-data))
                 (generated-regexp-list-p (cdr initial-regexp-list-data))
                 (initial-data
                  (nucleo-completion--initial-completion-data
                   prefix pattern table pred completion-regexp-list
                   generated-regexp-list-p)))
            (pcase-let ((`(,all . ,base-size) initial-data))
              (unless (equal prefix nucleo-completion--current-prefix)
                (nucleo-completion--clear-completion-state prefix nil))
              (pcase-let* ((`(,all ,_bundle ,top-info ,full-scores)
                            (nucleo-completion--filter-completions
                             prefix pattern all module-p expanded-regexp-p
                             highlight-limit need-full-scores))
                           (max-score (nucleo-completion--max-score
                                       top-info full-scores)))
                (if all
                    (progn
                      (nucleo-completion--record-current-result
                       prefix pattern all base-size)
                      (setq all (nucleo-completion--highlight-completions
                                 pattern all top-info max-score
                                 highlight-limit lazy-hilit-p))
                      (nucleo-completion--with-base-size prefix all base-size))
                  (nucleo-completion--reset-completion-state)
                  nil)))))))))

;;;###autoload
(defun nucleo-completion-all-completions (string table &optional pred point)
  "Get Nucleo completions of STRING in TABLE.
PRED and POINT follow `completion-all-completions' semantics.
Wrap filtering with `while-no-input' so interactive typing can
interrupt expensive scoring."
  (pcase (while-no-input
           (catch 'nucleo-completion-interrupted
             (nucleo-completion--all-completions-1
              string table pred point)))
    ('nil nil)
    ('t
     (when (consp nucleo-completion--current-result)
       (let ((result (copy-sequence nucleo-completion--current-result)))
         (nucleo-completion--with-base-size
          nucleo-completion--current-prefix
          result
          nucleo-completion--current-base-size))))
    (result result)))

;;;###autoload
(defun nucleo-completion-adjust-metadata (metadata)
  "Adjust completion METADATA for Nucleo sorting."
  (if nucleo-completion--filtering-p
      `(metadata (display-sort-function . identity)
                 (cycle-sort-function . identity)
                 . ,(cdr metadata))
    metadata))

;;;###autoload
(progn
  (add-to-list 'completion-styles-alist
               '(nucleo nucleo-completion-try-completion
                        nucleo-completion-all-completions
                        "Fuzzy completion backed by nucleo-matcher."))
  (put 'nucleo 'completion--adjust-metadata
       #'nucleo-completion-adjust-metadata))

(nucleo-completion--maybe-prompt-module-install)

(provide 'nucleo-completion)
;;; nucleo-completion.el ends here
