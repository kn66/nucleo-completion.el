;;; nucleo-completion.el --- Nucleo-backed completion style  -*- lexical-binding: t -*-

;; Copyright (C) 2026

;; Author: Nobu <https://github.com/kn66>
;; Assisted-by: OpenAI Codex:gpt-5
;; Version: 0.1.11
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
(declare-function nucleo-completion-candidates "nucleo-completion-module")
(declare-function nucleo-completion-candidates-with-history
                  "nucleo-completion-module")
(declare-function mm-destroy-parts "mm-decode")
(declare-function mm-dissect-buffer "mm-decode")
(declare-function mm-save-part-to-file "mm-decode")
(declare-function url-retrieve-synchronously "url")
(defvar mm-attachment-file-modes)
(defvar completion-flex-nospace)
(defvar completion-lazy-hilit-fn)
(defvar completion-regexp-list)
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

(defcustom nucleo-completion-regexp-minimum-term-length 2
  "Minimum search term length for calling regexp expanders.
Terms shorter than this value use only the built-in fuzzy matcher.
The default avoids broad one-character regexp expansions from tools
such as Migemo.  Set this to 1 to run regexp expanders for
single-character terms too."
  :type 'natnum
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
`unicode-string-p' error, Nucleo retries with scrubbing enabled
and keeps that path for subsequent calls."
  :type 'boolean
  :group 'nucleo-completion)

(defvar nucleo-completion--force-scrub-non-unicode-candidates nil
  "Non-nil after the module rejects a candidate as non-Unicode.")

(defcustom nucleo-completion-sort-ties-by-history nil
  "Whether to sort equal-scoring Nucleo matches by minibuffer history.
When non-nil, candidates found earlier in the current minibuffer
history come first.  This only affects candidates with the same
Nucleo score."
  :type 'boolean
  :group 'nucleo-completion)

(defcustom nucleo-completion-sort-ties-by-length nil
  "Whether to sort equal-scoring Nucleo matches by candidate length.
When non-nil, shorter candidates come first.  This only affects
candidates with the same Nucleo score."
  :type 'boolean
  :group 'nucleo-completion)

(defcustom nucleo-completion-sort-ties-alphabetically nil
  "Whether to sort equal-scoring Nucleo matches alphabetically.
When `nucleo-completion-sort-ties-by-length' is also non-nil,
alphabetical order is used after comparing length.  This only
affects candidates with the same Nucleo score."
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

(defcustom nucleo-completion-module-release-tag nil
  "GitHub Release tag used by `nucleo-completion-install-module'.
When nil, the installer downloads from GitHub's latest release URL."
  :type '(choice (const :tag "Latest release" nil)
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

(defvar nucleo-completion--filtering-p nil)

(defvar-local nucleo-completion--current-result nil
  "Most recent unbased completion result produced during filtering.")

(defvar-local nucleo-completion--current-prefix ""
  "Prefix corresponding to `nucleo-completion--current-result'.")

(defvar nucleo-completion-module-load-errors nil
  "Dynamic module load failures from the last load attempt.
Each entry has the form (FILE . MESSAGE).")

(defvar nucleo-completion--module-install-prompted nil
  "Non-nil after an automatic module install prompt has been considered.")

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

(defun nucleo-completion--module-release-directory-name ()
  "Return the local directory name for the configured module release."
  (if (and (stringp nucleo-completion-module-release-tag)
           (not (string-empty-p nucleo-completion-module-release-tag)))
      nucleo-completion-module-release-tag
    "latest"))

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

(defun nucleo-completion--installed-module-directories ()
  "Return installed module directories for the current platform."
  (let ((triple (nucleo-completion--module-install-triple)))
    (when triple
      (list (nucleo-completion--module-install-directory triple)))))

(defun nucleo-completion--dynamic-modules-supported-p ()
  "Return non-nil when this Emacs can load dynamic modules."
  (and (fboundp 'module-load)
       (boundp 'module-file-suffix)))

(defun nucleo-completion--module-candidates ()
  "Return candidate paths for the Rust dynamic module."
  (when (nucleo-completion--dynamic-modules-supported-p)
    (let ((library (nucleo-completion--rust-library-name)))
      (delq nil
            (append
             (list
              (locate-library "nucleo-completion-module" nil (list module-file-suffix))
              (locate-library library))
             (mapcar (lambda (dir)
                       (expand-file-name library dir))
                     (append
                      (nucleo-completion--installed-module-directories)
                      (nucleo-completion--prebuilt-module-directories)
                      (list nucleo-completion--directory
                            (expand-file-name "target/release" nucleo-completion--directory)
                            (expand-file-name "target/debug" nucleo-completion--directory)))))))))

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
                  (setq loaded t)
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
          (if (and (stringp nucleo-completion-module-release-tag)
                   (not (string-empty-p nucleo-completion-module-release-tag)))
              (concat "download/" nucleo-completion-module-release-tag)
            "latest/download")))

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

(defun nucleo-completion--flex-nospace-p (string)
  "Return non-nil when built-in flex settings reject STRING."
  (and (bound-and-true-p completion-flex-nospace)
       (string-match-p " " string)))

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
       (condition-case nil
           (progn
             (string-match-p regexp "")
             t)
         (invalid-regexp nil))))

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
             for value = (when (functionp function)
                           (ignore-errors (funcall function term)))
             append (cond
                     ((nucleo-completion--valid-regexp-p value)
                      (list value))
                     ((listp value)
                      (cl-remove-if-not
                       #'nucleo-completion--valid-regexp-p value))))))

(defun nucleo-completion--term-regexps (term)
  "Return regexps that may match TERM."
  (let ((fuzzy (concat "\\`" (nucleo-completion--subsequence-regexp term))))
    (cons fuzzy (nucleo-completion--regexp-function-regexps term))))

(defun nucleo-completion--term-regexp-groups (needle)
  "Return regexp groups for NEEDLE.
Each group corresponds to one term.  A candidate must match at
least one regexp from every group."
  (mapcar (lambda (term)
            (nucleo-completion--term-regexps term))
          (nucleo-completion--terms needle)))

(defun nucleo-completion--expanded-regexp-p (needle)
  "Return non-nil if NEEDLE has extra regexps from configured functions."
  (cl-some #'nucleo-completion--regexp-function-regexps
           (nucleo-completion--terms needle)))

(defun nucleo-completion--regexp-only-term-regexps (term)
  "Return regexps for TERM in the module regexp-only pass.
When TERM has configured regexp expansions, return only those
regexps because the Rust module has already handled fuzzy matching.
Terms without configured expansions still use the fuzzy matcher so
they can combine with expanded terms."
  (or (nucleo-completion--regexp-function-regexps term)
      (list (concat "\\`" (nucleo-completion--subsequence-regexp term)))))

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
  (mapcar #'car
          (nucleo-completion--regexp-filter-pairs needle candidates)))

(defun nucleo-completion--regexp-filter-pairs (needle candidates)
  "Return CANDIDATES matching NEEDLE as (CANDIDATE . nil) pairs."
  (let ((case-fold-search completion-ignore-case)
        (regexp-groups (nucleo-completion--term-regexp-groups needle)))
    (cl-loop for candidate in candidates
             when (nucleo-completion--regexp-match-p
                   regexp-groups candidate)
             collect (cons candidate nil))))

(defun nucleo-completion--fallback-filter (needle candidates)
  "Filter CANDIDATES against NEEDLE without scoring or sorting."
  (nucleo-completion--regexp-filter needle candidates))

(defun nucleo-completion--module-filter (needle candidates ignore-case)
  "Filter and sort CANDIDATES against NEEDLE with the Rust module.
Honor IGNORE-CASE."
  (let ((completion-ignore-case ignore-case))
    (car (nucleo-completion--module-completion-results
          "" needle candidates nil 0 nil))))

(defun nucleo-completion--module-filter-with-scores
    (needle candidates ignore-case)
  "Filter CANDIDATES against NEEDLE with the Rust module and keep scores.
Honor IGNORE-CASE.  The result is an alist of (CANDIDATE . SCORE)."
  (let ((completion-ignore-case ignore-case))
    (pcase-let ((`(,cands ,_bundle ,_top-info ,scores)
                 (nucleo-completion--module-completion-results
                  "" needle candidates nil 0 t)))
      (cl-mapcar #'cons cands scores))))

(defun nucleo-completion--module-ready-p ()
  "Return non-nil when the current Rust module provides the batch API."
  (fboundp 'nucleo-completion-candidates))

(defun nucleo-completion--module-supports-history-p ()
  "Return non-nil when the Rust module supports history tie sorting."
  (fboundp 'nucleo-completion-candidates-with-history))

;;;###autoload
(defun nucleo-completion-install-module (&optional force no-confirm)
  "Download and install the Rust dynamic module for this platform.
The module is downloaded from GitHub Releases only after
confirmation, unless NO-CONFIRM is non-nil.  With prefix argument
FORCE, replace an existing installed module."
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
copy to the original candidate.  When no scrubbing is requested,
or no candidate needs substitution, the original CANDIDATES list
is returned unchanged and MAP is nil."
  (let ((scrub-non-unicode
         (or scrub-non-unicode
             nucleo-completion-scrub-non-unicode-candidates
             nucleo-completion--force-scrub-non-unicode-candidates)))
    (if (not scrub-non-unicode)
        (cons candidates nil)
      (let (map cleaned pairs)
        (dolist (candidate candidates)
          (let ((scrubbed
                 (nucleo-completion--scrub-non-unicode-string candidate)))
            (push scrubbed cleaned)
            (unless (eq scrubbed candidate)
              (unless map
                (setq map (make-hash-table :test #'eq
                                           :size (length candidates))))
              (puthash scrubbed candidate map)
              (push (cons scrubbed candidate) pairs))))
        (when pairs
          (puthash nucleo-completion--scrub-map-pairs-key
                   (nreverse pairs) map))
        (cons (if map (nreverse cleaned) candidates) map)))))

(defun nucleo-completion--scrub-map-queues (map)
  "Return fresh equal-keyed restore queues for MAP."
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
                        (nucleo-completion--restore-scrubbed-candidate
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
    (if map
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
non-Unicode, this function retries once with scrubbing enabled and
uses that path for subsequent calls.
Original candidate strings are restored on the way back so text
properties (Consult metadata, invisibility, faces) survive when a
scrubbed copy was passed to the module."
  (let ((scrub-non-unicode
         (or nucleo-completion-scrub-non-unicode-candidates
             nucleo-completion--force-scrub-non-unicode-candidates)))
    (condition-case err
        (nucleo-completion--module-results-with-scrub
         needle candidates ignore-case highlight-limit return-all-scores
         scrub-non-unicode history-ranks)
      (wrong-type-argument
       (if (and (not scrub-non-unicode)
                (nucleo-completion--module-unicode-error-p err))
           (progn
             (setq nucleo-completion--force-scrub-non-unicode-candidates t)
             (nucleo-completion--module-results-with-scrub
              needle candidates ignore-case highlight-limit return-all-scores
              t history-ranks))
         (signal (car err) (cdr err)))))))

(defun nucleo-completion--bundle-candidates (bundle)
  "Return the candidate list from BUNDLE."
  (nth 0 bundle))

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

(defun nucleo-completion--top-info-hash (top-info)
  "Return a hash table mapping candidates in TOP-INFO to entries."
  (let ((table (make-hash-table :test #'equal :size (length top-info))))
    (dolist (entry top-info)
      (puthash (nucleo-completion--top-info-candidate entry) entry table))
    table))

(defun nucleo-completion--full-scores-hash (candidates full-scores)
  "Return hash table mapping CANDIDATES to FULL-SCORES.
Both lists must be the same length."
  (let ((table (make-hash-table :test #'equal :size (length candidates))))
    (cl-mapc (lambda (cand score) (puthash cand score table))
             candidates full-scores)
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

(defun nucleo-completion--score-properties-present-p (candidates full-scores)
  "Return non-nil when CANDIDATES already carry FULL-SCORES."
  (or (null full-scores)
      (and candidates
           (nucleo-completion--candidate-score (car candidates)))))

(defun nucleo-completion--ensure-score-properties (bundle)
  "Ensure BUNDLE candidates carry score text properties.
Newer Rust modules attach scores while constructing the candidate
list.  This fallback preserves lazy score-band highlighting with
older modules and mocked module bundles without building an
`equal' hash table keyed by candidate strings."
  (let ((candidates (nucleo-completion--bundle-candidates bundle))
        (full-scores (nucleo-completion--bundle-full-scores bundle)))
    (unless (nucleo-completion--score-properties-present-p
             candidates full-scores)
      (setcar bundle
              (cl-mapcar #'nucleo-completion--propertize-score
                         candidates full-scores))))
  bundle)

(defun nucleo-completion--history-rank-less-p
    (candidate-a candidate-b rank-table)
  "Return non-nil when CANDIDATE-A has a better history rank than CANDIDATE-B."
  (let ((rank-a (gethash candidate-a rank-table))
        (rank-b (gethash candidate-b rank-table)))
    (cond
     ((and rank-a rank-b) (< rank-a rank-b))
     (rank-a t)
     (rank-b nil)
     (t nil))))

(defun nucleo-completion--history-sort-before-p
    (candidate-a score-a position-a candidate-b score-b position-b rank-table)
  "Return non-nil when A should sort before B for history tie sorting.
CANDIDATE-A, SCORE-A, and POSITION-A describe A.  CANDIDATE-B,
SCORE-B, and POSITION-B describe B.  RANK-TABLE maps candidates
to history ranks."
  (let ((score-a (or score-a 0))
        (score-b (or score-b 0)))
    (cond
     ((/= score-a score-b) (> score-a score-b))
     ((nucleo-completion--history-rank-less-p
       candidate-a candidate-b rank-table)
      t)
     ((nucleo-completion--history-rank-less-p
       candidate-b candidate-a rank-table)
      nil)
     (t (< position-a position-b)))))

(defun nucleo-completion--sort-top-info-ties-by-history
    (top-info rank-table)
  "Sort TOP-INFO by score and history rank using RANK-TABLE."
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
         rank-table))))))

(defun nucleo-completion--sort-bundle-ties-by-history
    (bundle rank-table)
  "Sort equal-score candidates in BUNDLE by history using RANK-TABLE.
This is used for older Rust modules that do not provide the
history-aware batch API.  BUNDLE must include FULL-SCORES."
  (pcase-let ((`(,candidates ,top-info ,full-scores) bundle))
    (if (or (null candidates) (null full-scores))
        bundle
      (let ((items
             (sort
              (cl-loop for candidate in candidates
                       for score in full-scores
                       for position from 0
                       collect (list candidate score position))
              (lambda (a b)
                (nucleo-completion--history-sort-before-p
                 (nth 0 a) (nth 1 a) (nth 2 a)
                 (nth 0 b) (nth 1 b) (nth 2 b)
                 rank-table)))))
        (list (mapcar #'car items)
              (nucleo-completion--sort-top-info-ties-by-history
               top-info rank-table)
              (mapcar #'cadr items))))))

(defun nucleo-completion--regexp-only-candidates
    (needle scorable module-candidates)
  "Return SCORABLE candidates not in MODULE-CANDIDATES that match NEEDLE regexps.
Walk SCORABLE once."
  (let ((seen (make-hash-table :test #'equal
                               :size (max 1 (length module-candidates))))
        (case-fold-search completion-ignore-case)
        (regexp-groups (nucleo-completion--regexp-only-regexp-groups needle))
        result)
    (dolist (candidate module-candidates)
      (puthash candidate t seen))
    (dolist (candidate scorable)
      (unless (gethash candidate seen)
        (when (nucleo-completion--regexp-match-p regexp-groups candidate)
          (push candidate result))))
    (nreverse result)))

(defun nucleo-completion--prepend-regexp-only-matches
    (needle scorable bundle)
  "Return BUNDLE with regexp-only matches from SCORABLE prepended.
NEEDLE is the current completion pattern.
BUNDLE is (CANDIDATES TOP-INFO FULL-SCORES).  Returns a new
bundle with the same shape.  For each prepended candidate a
synthetic top-info entry (CAND MAX-SCORE nil) is added so the
score-band highlighter still classifies these matches in the high
band, and FULL-SCORES, when present, is extended with MAX-SCORE
entries to keep the parallel-array invariant."
  (pcase-let* ((`(,module-candidates ,top-info ,full-scores) bundle)
               (max-score (or (and top-info
                                   (nucleo-completion--top-info-score
                                    (car top-info)))
                              1))
               (extra (nucleo-completion--regexp-only-candidates
                       needle scorable module-candidates)))
    (if (null extra)
        bundle
      (when full-scores
        (setq extra
              (mapcar (lambda (cand)
                        (nucleo-completion--propertize-score cand max-score))
                      extra)))
      (list (append extra module-candidates)
            (append (mapcar (lambda (cand) (list cand max-score nil)) extra)
                    top-info)
            (when full-scores
              (append (mapcar (lambda (_) max-score) extra)
                      full-scores))))))

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
  (or (nucleo-completion--exact-word-match-p needle candidate)
      (and max-score
           (> max-score 0)
           (>= score (* max-score
                        (nucleo-completion--high-score-ratio))))))

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
  (let ((case-fold-search completion-ignore-case))
    (if indices
        (dolist (index indices)
          (when (< index (length haystack))
            (add-face-text-property index (1+ index)
                                    'completions-common-part nil haystack)))
      (dolist (term (nucleo-completion--terms needle))
        (let ((start 0))
          (cl-loop for ch across term
                   for match = (string-match (regexp-quote (char-to-string ch))
                                             haystack start)
                   while match
                   do (add-face-text-property match (1+ match)
                                              'completions-common-part nil haystack)
                   (setq start (1+ match))))))
    (dolist (term (nucleo-completion--terms needle))
      (dolist (regexp (nucleo-completion--regexp-function-regexps term))
        (nucleo-completion--highlight-regexp regexp haystack))))
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
     (apply #'append (nucleo-completion--term-regexp-groups needle))
     completion-regexp-list)))

(defun nucleo-completion--initial-completion-candidates
    (prefix needle table pred regexp-list)
  "Return completion candidates before Nucleo filtering.
PREFIX, NEEDLE, TABLE, and PRED have the same meaning as in
`nucleo-completion--all-completions-1'.  REGEXP-LIST is the
currently effective `completion-regexp-list'."
  (if (and (string= prefix "") (stringp (car-safe table))
           (not (or pred regexp-list
                    (string= needle ""))))
      table
    (let ((completion-regexp-list regexp-list))
      (all-completions prefix table pred))))

(defun nucleo-completion--active-history-variable-p ()
  "Return non-nil when history tie sorting has usable history state."
  (and nucleo-completion-sort-ties-by-history
       (minibufferp)
       (boundp 'minibuffer-history-variable)
       (symbolp minibuffer-history-variable)
       (not (eq minibuffer-history-variable t))
       (boundp minibuffer-history-variable)))

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

(defun nucleo-completion--history-rank-table (prefix)
  "Return a hash table mapping history candidates under PREFIX to ranks."
  (when (nucleo-completion--active-history-variable-p)
    (let* ((history (symbol-value minibuffer-history-variable))
           (entries (if (listp history) history nil))
           (default (nucleo-completion--history-default))
           (table (make-hash-table :test #'equal
                                   :size (max 1 (length entries))))
           (rank 0))
      (dolist (entry (if default (cons default entries) entries))
        (let ((candidate
               (nucleo-completion--history-entry-candidate prefix entry)))
          (when (and candidate
                     (eq (gethash candidate table :missing) :missing))
            (puthash candidate rank table)
            (setq rank (1+ rank)))))
      (when (> rank 0)
        table))))

(defun nucleo-completion--history-ranking (prefix candidates)
  "Return (RANKS . TABLE) for CANDIDATES under PREFIX, or nil.
RANKS is parallel to CANDIDATES and contains integers for
history entries and nil for candidates not present in history.
TABLE maps candidate strings to their history ranks."
  (let ((table (nucleo-completion--history-rank-table prefix)))
    (when table
      (let (ranks any)
        (dolist (candidate candidates)
          (let ((rank (gethash candidate table)))
            (when rank
              (setq any t))
            (push rank ranks)))
        (when any
          (cons (nreverse ranks) table))))))

(defun nucleo-completion--module-completion-results
    (prefix needle all expanded-regexp-p highlight-limit need-full-scores)
  "Return module-backed filtering result for NEEDLE and ALL.
PREFIX is used to interpret minibuffer history entries.  When
EXPANDED-REGEXP-P is non-nil, prepend regexp-only matches.
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
    (when (and history-rank-table (not module-history-p))
      (setq bundle
            (nucleo-completion--sort-bundle-ties-by-history
             bundle history-rank-table)))
    (when need-full-scores
      (setq bundle (nucleo-completion--ensure-score-properties bundle)))
    (when expanded-regexp-p
      (setq bundle
            (nucleo-completion--prepend-regexp-only-matches
             needle all bundle)))
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
    (list (nucleo-completion--fallback-filter needle all) nil nil nil))))

(defun nucleo-completion--record-current-result (prefix needle all)
  "Record ALL as the current completion result for PREFIX and NEEDLE."
  (let ((prefix (if (stringp prefix) prefix "")))
    (setq nucleo-completion--filtering-p (not (string= needle "")))
    (setq nucleo-completion--current-prefix prefix
          nucleo-completion--current-result
          (if (string= prefix "") all (copy-sequence all)))))

(defun nucleo-completion--install-lazy-highlight
    (needle bundle top-info full-scores max-score)
  "Install lazy highlighting for NEEDLE using BUNDLE metadata.
TOP-INFO and FULL-SCORES provide score and index lookup data.
MAX-SCORE is used for score-band classification."
  (ignore bundle full-scores)
  (let ((info-hash (and top-info
                        (nucleo-completion--top-info-hash top-info))))
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
              (nucleo-completion--highlight-candidate
               needle candidate score max-score indices))))))

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
        (setcar cell
                (nucleo-completion--highlight-candidate
                 needle (copy-sequence candidate) score max-score indices)))
      (setq cell (cdr cell)
            remaining (1- remaining))))
  all)

(defun nucleo-completion--highlight-completions
    (needle all bundle top-info full-scores max-score highlight-limit lazy-hilit-p)
  "Apply lazy or eager highlighting setup to ALL.
NEEDLE is the current completion pattern.  BUNDLE, TOP-INFO,
FULL-SCORES, MAX-SCORE, and HIGHLIGHT-LIMIT provide module
metadata.  LAZY-HILIT-P selects lazy highlighting."
  (cond
   (lazy-hilit-p
    (nucleo-completion--install-lazy-highlight
     needle bundle top-info full-scores max-score)
    all)
   ((and (> highlight-limit 0) all)
    (nucleo-completion--highlight-eager
     needle all top-info max-score highlight-limit))
   (t all)))

(defun nucleo-completion--with-base-size (prefix all)
  "Return ALL with a base-size marker when PREFIX is non-empty."
  (and all (if (string= prefix "") all (nconc all (length prefix)))))

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

(defun nucleo-completion--try-result
    (string point prefix pattern suffix candidates)
  "Return a `try-completion' result from filtered CANDIDATES.
STRING and POINT are the original completion input.  PREFIX and
SUFFIX are outside the completion field.  PATTERN is the full
completion field text used for filtering.  When multiple
candidates exist but their common prefix does not extend PATTERN,
return STRING unchanged at POINT to indicate that matches exist."
  (let ((plain-candidates
         (delete-dups (mapcar #'substring-no-properties candidates))))
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
    (string table pred point)
  "Try completing STRING by using Nucleo's filtered candidate set.
TABLE is the completion table.  PRED and POINT follow
`completion-try-completion' semantics."
  (pcase-let* ((`(,point ,prefix ,_before ,_field-suffix ,suffix ,pattern)
                (nucleo-completion--field-state string table pred point))
               (module-p (nucleo-completion--module-ready-p))
               (expanded-regexp-p
                (nucleo-completion--expanded-regexp-p pattern))
               (completion-regexp-list
                (nucleo-completion--completion-regexp-list
                 pattern expanded-regexp-p module-p))
               (all (nucleo-completion--initial-completion-candidates
                     prefix pattern table pred completion-regexp-list)))
    (pcase-let ((`(,all ,_bundle ,_top-info ,_full-scores)
                 (nucleo-completion--filter-completions
                  prefix pattern all module-p expanded-regexp-p 0 nil)))
      (nucleo-completion--record-current-result prefix pattern all)
      (nucleo-completion--refine-try-result
       (nucleo-completion--try-result
        string point prefix pattern suffix all)
       string point table pred))))

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
  (pcase-let ((`(,point ,_prefix ,_before ,_field-suffix ,_suffix ,pattern)
               (nucleo-completion--field-state string table pred point)))
    (unless (nucleo-completion--flex-nospace-p string)
      (let ((expanded-regexp-p
             (nucleo-completion--expanded-regexp-p pattern)))
        (if (nucleo-completion--complex-try-p pattern expanded-regexp-p)
            (nucleo-completion--try-completion-with-filter
             string table pred point)
          (let ((result (completion-flex-try-completion
                         string table pred point)))
            (if result
                (progn
                  (setq nucleo-completion--filtering-p
                        (not (string= pattern "")))
                  (nucleo-completion--refine-try-result
                   result string point table pred))
              (nucleo-completion--try-completion-with-filter
               string table pred point))))))))

;;;###autoload
(defun nucleo-completion-try-completion (string table pred point)
  "Try completing STRING in TABLE with the Nucleo completion style.
PRED and POINT follow `completion-try-completion' semantics."
  (let* ((nucleo-completion--terms-cache (make-hash-table :test #'equal))
         (nucleo-completion--subsequence-regexp-cache
          (make-hash-table :test #'equal))
         (nucleo-completion--regexp-cache
          (and nucleo-completion-regexp-functions
               (make-hash-table :test #'equal))))
    (nucleo-completion--try-completion-1 string table pred point)))

(defun nucleo-completion--all-completions-1 (string table &optional pred point)
  "Get Nucleo completions of STRING in TABLE.
See `completion-all-completions' for the semantics of PRED and POINT."
  (let* ((nucleo-completion--terms-cache (make-hash-table :test #'equal))
         (nucleo-completion--subsequence-regexp-cache
          (make-hash-table :test #'equal))
         (nucleo-completion--regexp-cache
          (and nucleo-completion-regexp-functions
               (make-hash-table :test #'equal)))
         (nucleo-completion--exact-word-regexp-cache
          (and nucleo-completion-highlight-score-bands
               (make-hash-table :test #'equal))))
    (pcase-let ((`(,_point ,prefix ,_before ,_field-suffix ,_suffix ,pattern)
                 (nucleo-completion--field-state string table pred point)))
      (unless (nucleo-completion--flex-nospace-p string)
        (let* ((module-p (nucleo-completion--module-ready-p))
               (expanded-regexp-p
                (nucleo-completion--expanded-regexp-p pattern))
               (highlight-limit (nucleo-completion--highlight-limit))
               (lazy-hilit-p (bound-and-true-p completion-lazy-hilit))
               (need-full-scores
                (and lazy-hilit-p nucleo-completion-highlight-score-bands))
               (completion-regexp-list
                (nucleo-completion--completion-regexp-list
                 pattern expanded-regexp-p module-p))
               (all (nucleo-completion--initial-completion-candidates
                     prefix pattern table pred completion-regexp-list)))
          (unless (equal prefix nucleo-completion--current-prefix)
            (setq nucleo-completion--current-prefix prefix
                  nucleo-completion--current-result nil))
          (pcase-let* ((`(,all ,bundle ,top-info ,full-scores)
                        (nucleo-completion--filter-completions
                         prefix pattern all module-p expanded-regexp-p
                         highlight-limit need-full-scores))
                       (max-score (and top-info
                                       (nucleo-completion--top-info-score
                                        (car top-info)))))
            (nucleo-completion--record-current-result prefix pattern all)
            (setq all (nucleo-completion--highlight-completions
                       pattern all bundle top-info full-scores max-score
                       highlight-limit lazy-hilit-p))
            (nucleo-completion--with-base-size prefix all)))))))

;;;###autoload
(defun nucleo-completion-all-completions (string table &optional pred point)
  "Get Nucleo completions of STRING in TABLE.
PRED and POINT follow `completion-all-completions' semantics.
Wrap filtering with `while-no-input' so interactive typing can
interrupt expensive scoring."
  (pcase (while-no-input
           (nucleo-completion--all-completions-1 string table pred point))
    ('nil nil)
    ('t
     (when (consp nucleo-completion--current-result)
       (let ((result (copy-sequence nucleo-completion--current-result)))
         (if (string= nucleo-completion--current-prefix "")
             result
           (nconc result (length nucleo-completion--current-prefix))))))
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
