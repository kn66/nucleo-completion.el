;;; nucleo-completion.el --- Nucleo-backed completion style  -*- lexical-binding: t -*-

;; Copyright (C) 2026

;; Author: Nobu <https://github.com/kn66>
;; Assisted-by: OpenAI Codex
;; Version: 0.1.6
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

(declare-function nucleo-completion-candidates "nucleo-completion-module")

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

(defcustom nucleo-completion-persistent-regexp-cache-size nil
  "Maximum number of regexp expander results cached across completion passes.
When nil, regexp expander results are only cached for one
completion pass.  When set to a positive integer, results are
also cached across completion passes.  Set this only when your
regexp functions return stable results for the same term,
function list, `completion-ignore-case', and `default-directory'."
  :type '(choice (const :tag "Disable persistent cache" nil)
                 (natnum :tag "Maximum cached entries"))
  :group 'nucleo-completion)

(defcustom nucleo-completion-long-candidate-threshold nil
  "Maximum candidate length to score with the Rust module.
Candidates longer than this number of characters are filtered but
not scored.  Long matching candidates are appended after scored
matches in their original order.  Set this to nil to score every
candidate regardless of length, which avoids an Emacs Lisp
pre-scan over every candidate before each module call.

This value also acts as the default ceiling for regexp expander
matching and match highlighting when
`nucleo-completion-long-candidate-regexp-threshold' or
`nucleo-completion-long-candidate-highlight-threshold' are left
at their `inherit' default."
  :type '(choice (const :tag "Score every candidate" nil)
                 (natnum :tag "Maximum scored candidate length"))
  :group 'nucleo-completion)

(defcustom nucleo-completion-long-candidate-regexp-threshold 'inherit
  "Maximum candidate length to match with regexp expanders.
Candidates longer than this number of characters only use the
built-in fuzzy subsequence matcher; regexps from
`nucleo-completion-regexp-functions' are skipped during filtering
and highlighting.

When set to the symbol `inherit' (the default) the threshold
follows `nucleo-completion-long-candidate-threshold'.  Set this
to nil to use regexp expanders for every candidate regardless of
length, or to a positive integer to override the inherited
threshold."
  :type '(choice (const :tag "Inherit from `nucleo-completion-long-candidate-threshold'"
                        inherit)
                 (const :tag "Use regexp expanders for every candidate" nil)
                 (natnum :tag "Maximum regexp-expanded candidate length"))
  :group 'nucleo-completion)

(defcustom nucleo-completion-long-candidate-highlight-threshold 'inherit
  "Maximum candidate length to highlight.
Candidates longer than this number of characters are returned
without match highlighting.

When set to the symbol `inherit' (the default) the threshold
follows `nucleo-completion-long-candidate-threshold'.  Set this
to nil to highlight every candidate regardless of length, or to
a positive integer to override the inherited threshold."
  :type '(choice (const :tag "Inherit from `nucleo-completion-long-candidate-threshold'"
                        inherit)
                 (const :tag "Highlight every candidate" nil)
                 (natnum :tag "Maximum highlighted candidate length"))
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
module call."
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

(defvar nucleo-completion--regexp-cache nil
  "Cache for regexp expander results during one completion pass.")

(defvar nucleo-completion--terms-cache nil
  "Cache for split search terms during one completion pass.")

(defvar nucleo-completion--subsequence-regexp-cache nil
  "Cache for fuzzy subsequence regexps during one completion pass.")

(defvar nucleo-completion--exact-word-regexp-cache nil
  "Cache for exact word regexps during one completion pass.")

(defvar nucleo-completion--persistent-regexp-cache (make-hash-table :test #'equal)
  "Cache for regexp expander results across completion passes.")

(defconst nucleo-completion--directory
  (file-name-directory (or load-file-name byte-compile-current-file buffer-file-name))
  "Directory containing nucleo-completion.el.")

(defun nucleo-completion--string-key (string)
  "Return STRING unchanged.
Hash tables that key on candidate strings use the `equal' test,
which compares string contents independently of text properties.
Stripping properties via `substring-no-properties' would allocate
a fresh string per call without changing lookup semantics, so the
function is a no-op."
  string)

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
                      (nucleo-completion--prebuilt-module-directories)
                      (list nucleo-completion--directory
                            (expand-file-name "target/release" nucleo-completion--directory)
                            (expand-file-name "target/debug" nucleo-completion--directory)))))))))

(defun nucleo-completion--highlight-limit ()
  "Return a sanitized value for eager or module highlight limits."
  (if (and (integerp nucleo-completion-max-highlighted-completions)
           (>= nucleo-completion-max-highlighted-completions 0))
      nucleo-completion-max-highlighted-completions
    0))

(defun nucleo-completion--long-candidate-threshold ()
  "Return sanitized long-candidate scoring threshold, or nil."
  (when (and (integerp nucleo-completion-long-candidate-threshold)
             (>= nucleo-completion-long-candidate-threshold 0))
    nucleo-completion-long-candidate-threshold))

(defun nucleo-completion--long-candidate-p (candidate)
  "Return non-nil when CANDIDATE should skip module scoring."
  (let ((threshold (nucleo-completion--long-candidate-threshold)))
    (and threshold
         (> (length candidate) threshold))))

(defun nucleo-completion--long-candidate-regexp-threshold ()
  "Return sanitized long-candidate regexp threshold, or nil.
When the user setting is the symbol `inherit', the value is
resolved from `nucleo-completion-long-candidate-threshold'."
  (let ((value (if (eq nucleo-completion-long-candidate-regexp-threshold
                       'inherit)
                   nucleo-completion-long-candidate-threshold
                 nucleo-completion-long-candidate-regexp-threshold)))
    (when (and (integerp value) (>= value 0))
      value)))

(defun nucleo-completion--long-candidate-regexp-p (candidate)
  "Return non-nil when CANDIDATE should skip regexp expanders."
  (let ((threshold (nucleo-completion--long-candidate-regexp-threshold)))
    (and threshold
         (> (length candidate) threshold))))

(defun nucleo-completion--long-candidate-highlight-threshold ()
  "Return sanitized long-candidate highlight threshold, or nil.
When the user setting is the symbol `inherit', the value is
resolved from `nucleo-completion-long-candidate-threshold'."
  (let ((value (if (eq nucleo-completion-long-candidate-highlight-threshold
                       'inherit)
                   nucleo-completion-long-candidate-threshold
                 nucleo-completion-long-candidate-highlight-threshold)))
    (when (and (integerp value) (>= value 0))
      value)))

(defun nucleo-completion--long-candidate-highlight-p (candidate)
  "Return non-nil when CANDIDATE should skip match highlighting."
  (let ((threshold (nucleo-completion--long-candidate-highlight-threshold)))
    (and threshold
         (> (length candidate) threshold))))

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

(defun nucleo-completion--terms (pattern)
  "Split PATTERN into non-empty whitespace-separated terms."
  (if (hash-table-p nucleo-completion--terms-cache)
      (let ((cached (gethash pattern nucleo-completion--terms-cache :missing)))
        (if (not (eq cached :missing))
            cached
          (let ((terms (split-string pattern "[[:space:]]+" t)))
            (puthash pattern terms nucleo-completion--terms-cache)
            terms)))
    (split-string pattern "[[:space:]]+" t)))

(defun nucleo-completion--subsequence-regexp (needle)
  "Return a fuzzy subsequence regexp for NEEDLE."
  (if (hash-table-p nucleo-completion--subsequence-regexp-cache)
      (let ((cached (gethash needle nucleo-completion--subsequence-regexp-cache
                             :missing)))
        (if (not (eq cached :missing))
            cached
          (let ((regexp (nucleo-completion--subsequence-regexp-1 needle)))
            (puthash needle regexp nucleo-completion--subsequence-regexp-cache)
            regexp)))
    (nucleo-completion--subsequence-regexp-1 needle)))

(defun nucleo-completion--subsequence-regexp-1 (needle)
  "Return an uncached fuzzy subsequence regexp for NEEDLE."
  (mapconcat
   (lambda (ch)
     (let ((s (char-to-string ch)))
       (concat "[^" (regexp-quote s) "]*" (regexp-quote s))))
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
  (if (hash-table-p nucleo-completion--regexp-cache)
      (let ((cached (gethash term nucleo-completion--regexp-cache :missing)))
        (if (not (eq cached :missing))
            cached
          (let ((regexps (nucleo-completion--regexp-function-regexps-1 term)))
            (puthash term regexps nucleo-completion--regexp-cache)
            regexps)))
    (nucleo-completion--regexp-function-regexps-1 term)))

(defun nucleo-completion--regexp-function-regexps-1 (term)
  "Return persistent-cached extra regexps produced for TERM."
  (let ((limit (nucleo-completion--persistent-regexp-cache-limit)))
    (if limit
        (let* ((key (nucleo-completion--persistent-regexp-cache-key term))
               (cached (gethash key
                                nucleo-completion--persistent-regexp-cache
                                :missing)))
          (if (not (eq cached :missing))
              cached
            (let ((regexps
                   (nucleo-completion--regexp-function-regexps-uncached term)))
              (when (> limit 0)
                (when (>= (hash-table-count
                           nucleo-completion--persistent-regexp-cache)
                          limit)
                  (nucleo-completion-clear-persistent-regexp-cache))
                (puthash key regexps
                         nucleo-completion--persistent-regexp-cache))
              regexps)))
      (nucleo-completion--regexp-function-regexps-uncached term))))

(defun nucleo-completion--persistent-regexp-cache-limit ()
  "Return sanitized persistent regexp cache size, or nil when disabled."
  (when (and (integerp nucleo-completion-persistent-regexp-cache-size)
             (> nucleo-completion-persistent-regexp-cache-size 0))
    nucleo-completion-persistent-regexp-cache-size))

(defun nucleo-completion--persistent-regexp-cache-key (term)
  "Return persistent regexp cache key for TERM."
  (list term
        nucleo-completion-regexp-functions
        completion-ignore-case
        default-directory))

;;;###autoload
(defun nucleo-completion-clear-persistent-regexp-cache ()
  "Clear cached regexp expander results."
  (interactive)
  (clrhash nucleo-completion--persistent-regexp-cache))

(defun nucleo-completion--regexp-function-regexps-uncached (term)
  "Return uncached extra regexps produced for TERM."
  (cl-loop for function in nucleo-completion-regexp-functions
           for value = (when (functionp function)
                         (ignore-errors (funcall function term)))
           append (cond
                   ((nucleo-completion--valid-regexp-p value)
                    (list value))
                   ((listp value)
                    (cl-remove-if-not #'nucleo-completion--valid-regexp-p value)))))

(defun nucleo-completion--term-regexps (term &optional fuzzy-only)
  "Return regexps that may match TERM.
When FUZZY-ONLY is non-nil, omit regexps from
`nucleo-completion-regexp-functions'."
  (let ((fuzzy (concat "\\`" (nucleo-completion--subsequence-regexp term))))
    (if fuzzy-only
        (list fuzzy)
      (cons fuzzy (nucleo-completion--regexp-function-regexps term)))))

(defun nucleo-completion--term-regexp-groups (needle &optional fuzzy-only)
  "Return regexp groups for NEEDLE.
Each group corresponds to one term.  A candidate must match at
least one regexp from every group.  When FUZZY-ONLY is non-nil,
omit regexps from `nucleo-completion-regexp-functions'."
  (mapcar (lambda (term)
            (nucleo-completion--term-regexps term fuzzy-only))
          (nucleo-completion--terms needle)))

(defun nucleo-completion--expanded-regexp-p (needle)
  "Return non-nil if NEEDLE has extra regexps from configured functions."
  (cl-some (lambda (term)
             (nucleo-completion--regexp-function-regexps term))
           (nucleo-completion--terms needle)))

(defun nucleo-completion--regexp-match-p (regexp-groups candidate)
  "Return non-nil if CANDIDATE matches REGEXP-GROUPS."
  (cl-every (lambda (regexps)
              (cl-some (lambda (regexp)
                         (string-match-p regexp candidate))
                       regexps))
            regexp-groups))

(defun nucleo-completion--regexp-filter (needle candidates)
  "Filter CANDIDATES against NEEDLE with fuzzy and configured regexp matchers."
  (mapcar #'car
          (nucleo-completion--regexp-filter-pairs needle candidates)))

(defun nucleo-completion--regexp-filter-pairs (needle candidates)
  "Return CANDIDATES matching NEEDLE as (CANDIDATE . nil) pairs."
  (let ((case-fold-search completion-ignore-case)
        (regexp-groups (nucleo-completion--term-regexp-groups needle))
        fuzzy-regexp-groups)
    (cl-loop for candidate in candidates
             when (nucleo-completion--regexp-match-p
                   (if (nucleo-completion--long-candidate-regexp-p candidate)
                       (or fuzzy-regexp-groups
                           (setq fuzzy-regexp-groups
                                 (nucleo-completion--term-regexp-groups
                                  needle t)))
                     regexp-groups)
                   candidate)
             collect (cons candidate nil))))

(defun nucleo-completion--regexp-filter-pairs-excluding
    (needle candidates excluded)
  "Return CANDIDATES matching NEEDLE and absent from EXCLUDED.
EXCLUDED is a hash table keyed by `nucleo-completion--string-key'."
  (let ((case-fold-search completion-ignore-case)
        (regexp-groups (nucleo-completion--term-regexp-groups needle))
        fuzzy-regexp-groups)
    (cl-loop for candidate in candidates
             unless (gethash (nucleo-completion--string-key candidate)
                             excluded)
             when (nucleo-completion--regexp-match-p
                   (if (nucleo-completion--long-candidate-regexp-p candidate)
                       (or fuzzy-regexp-groups
                           (setq fuzzy-regexp-groups
                                 (nucleo-completion--term-regexp-groups
                                  needle t)))
                     regexp-groups)
                   candidate)
             collect (cons candidate nil))))

(defun nucleo-completion--split-scored-candidates (candidates)
  "Return (SCORABLE LONG) candidate lists split by length.
SCORABLE contains candidates that should be sent to the Rust
module.  LONG contains candidates that should only be filtered.
When no candidate exceeds the configured length threshold the
original CANDIDATES list is reused without copying."
  (let ((threshold (nucleo-completion--long-candidate-threshold)))
    (cond
     ((null threshold) (list candidates nil))
     ((not (cl-some (lambda (c) (> (length c) threshold)) candidates))
      (list candidates nil))
     (t
      (let (scorable long)
        (dolist (candidate candidates)
          (if (> (length candidate) threshold)
              (push candidate long)
            (push candidate scorable)))
        (list (nreverse scorable) (nreverse long)))))))

(defun nucleo-completion--fallback-filter (needle candidates)
  "Filter CANDIDATES against NEEDLE without scoring or sorting."
  (nucleo-completion--regexp-filter needle candidates))

(defun nucleo-completion--module-filter (needle candidates ignore-case)
  "Filter and sort CANDIDATES against NEEDLE with the Rust module.
Honor IGNORE-CASE."
  (car (nucleo-completion--module-results needle candidates ignore-case 0)))

(defun nucleo-completion--module-filter-with-scores
    (needle candidates ignore-case)
  "Filter CANDIDATES against NEEDLE with the Rust module and keep scores.
Honor IGNORE-CASE.  The result is an alist of (CANDIDATE . SCORE)."
  (let* ((bundle (nucleo-completion--module-results
                  needle candidates ignore-case 0 t))
         (cands (nth 0 bundle))
         (scores (nth 2 bundle)))
    (cl-mapcar #'cons cands scores)))

(defun nucleo-completion--module-ready-p ()
  "Return non-nil when the current Rust module provides the batch API."
  (fboundp 'nucleo-completion-candidates))

(defun nucleo-completion--module-supports-bundle-p ()
  "Return non-nil when the Rust module accepts the 7-argument bundle API.
Also returns non-nil for variadic stubs used in tests so they
exercise the bundle code path without re-implementing the
six-argument adapter."
  (let* ((arity (and (fboundp 'nucleo-completion-candidates)
                     (func-arity 'nucleo-completion-candidates)))
         (max-arity (and arity (cdr arity))))
    (or (eq max-arity 'many)
        (and (numberp max-arity) (>= max-arity 7)))))

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

(defun nucleo-completion--scrub-candidates (candidates)
  "Return (CLEANED . MAP) where CLEANED is suitable for the Rust module.
Walks CANDIDATES once and substitutes any string carrying
characters above `nucleo-completion--max-unicode-codepoint' with a
scrubbed copy.  MAP is a hash table from each substituted
scrubbed string to the original candidates in input order.  The
list value preserves distinct originals that scrub to the same
string.  When no candidate needs scrubbing the original CANDIDATES
list is returned unchanged and MAP is nil."
  (let (map cleaned)
    (dolist (candidate candidates)
      (let ((scrubbed (nucleo-completion--scrub-non-unicode-string candidate)))
        (push scrubbed cleaned)
        (unless (eq scrubbed candidate)
          (unless map
            (setq map (make-hash-table :test #'equal
                                       :size (length candidates))))
          (puthash scrubbed (cons candidate (gethash scrubbed map)) map))))
    (when map
      (maphash (lambda (scrubbed originals)
                 (puthash scrubbed (nreverse originals) map))
               map))
    (cons (if map (nreverse cleaned) candidates) map)))

(defun nucleo-completion--copy-scrub-map (map)
  "Return a copy of scrub MAP with independent queue lists."
  (let ((copy (make-hash-table :test #'equal :size (hash-table-count map))))
    (maphash (lambda (scrubbed originals)
               (puthash scrubbed (copy-sequence originals) copy))
             map)
    copy))

(defun nucleo-completion--restore-scrubbed-candidate (candidate map)
  "Return original candidate for scrubbed CANDIDATE using queue MAP."
  (let ((originals (gethash candidate map)))
    (if originals
        (prog1 (car originals)
          (puthash candidate (cdr originals) map))
      candidate)))

(defun nucleo-completion--restore-bundle-candidates (bundle map)
  "Replace scrubbed candidates inside BUNDLE with originals from MAP.
MAP values are queues in input order, so candidates that scrub to
the same string still restore to distinct originals.  Mutates
BUNDLE in place because the lists were freshly returned by the
Rust module and are not shared with any other caller."
  (let ((candidate-map (nucleo-completion--copy-scrub-map map))
        (top-info-map (nucleo-completion--copy-scrub-map map)))
    (cl-loop for cell on (nucleo-completion--bundle-candidates bundle)
             do (setcar cell
                        (nucleo-completion--restore-scrubbed-candidate
                         (car cell) candidate-map)))
    (dolist (entry (nucleo-completion--bundle-top-info bundle))
      (setcar entry
              (nucleo-completion--restore-scrubbed-candidate
               (car entry) top-info-map))))
  bundle)

(defun nucleo-completion--module-results
    (needle candidates ignore-case highlight-limit &optional return-all-scores)
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

Honor IGNORE-CASE.  Pre-bundle Rust modules (six-argument arity)
return a flat list of (CAND SCORE INDICES) triples; this function
adapts that legacy shape so callers can use one bundle layout.

Frontends such as Consult append \"tofu\" disambiguation characters
whose codepoints exceed Unicode's maximum, which the Rust module
cannot encode as UTF-8.  Such candidates are scrubbed of those
characters before being passed to the module and the original
candidate strings are restored on the way back so their text
properties (Consult metadata, invisibility, faces) survive when
`nucleo-completion-scrub-non-unicode-candidates' is non-nil."
  (pcase-let* ((`(,cleaned . ,map)
                (if nucleo-completion-scrub-non-unicode-candidates
                    (nucleo-completion--scrub-candidates candidates)
                  (cons candidates nil)))
               (bundle
                (if (nucleo-completion--module-supports-bundle-p)
                    (nucleo-completion-candidates
                     needle cleaned ignore-case
                     nucleo-completion-sort-ties-by-length
                     nucleo-completion-sort-ties-alphabetically
                     highlight-limit
                     return-all-scores)
                  (let ((triples
                         (nucleo-completion-candidates
                          needle cleaned ignore-case
                          nucleo-completion-sort-ties-by-length
                          nucleo-completion-sort-ties-alphabetically
                          highlight-limit)))
                    (list (mapcar #'car triples)
                          triples
                          (when return-all-scores
                            (mapcar #'cadr triples)))))))
    (if map
        (nucleo-completion--restore-bundle-candidates bundle map)
      bundle)))

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

(defun nucleo-completion--result-candidate (result)
  "Return RESULT's candidate."
  (car result))

(defun nucleo-completion--result-score (result)
  "Return RESULT's score."
  (cadr result))

(defun nucleo-completion--result-indices (result)
  "Return RESULT's precomputed highlight indices."
  (caddr result))

(defun nucleo-completion--results->scored (results)
  "Return RESULTS as (CANDIDATE . SCORE) pairs."
  (mapcar (lambda (result)
            (cons (nucleo-completion--result-candidate result)
                  (nucleo-completion--result-score result)))
          results))

(defun nucleo-completion--results->indices-table (results)
  "Return hash table mapping candidates in RESULTS to precomputed indices.
Only the first `nucleo-completion-max-highlighted-completions'
results are inspected because the Rust module only computes
indices for the top-ranked entries."
  (let ((table (make-hash-table :test #'equal))
        (limit (nucleo-completion--highlight-limit)))
    (cl-loop for result in results
             repeat limit
             for indices = (nucleo-completion--result-indices result)
             when indices
             do (puthash (nucleo-completion--result-candidate result)
                         indices table))
    table))

(defun nucleo-completion--results->candidate-table (results)
  "Return hash table containing candidates from RESULTS."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (result results)
      (puthash (nucleo-completion--string-key
                (nucleo-completion--result-candidate result))
               t table))
    table))

(defun nucleo-completion--merge-regexp-results (regexp-pairs module-results)
  "Merge REGEXP-PAIRS with MODULE-RESULTS.
Regexp-only candidates are promoted before module-scored results."
  (let ((seen (make-hash-table :test #'equal))
        regexp-results)
    (dolist (result module-results)
      (puthash (nucleo-completion--result-candidate result) t seen))
    (setq regexp-results
          (cl-loop with score = (or (nucleo-completion--result-score
                                     (car module-results))
                                    1)
                   for entry in regexp-pairs
                   unless (gethash (car entry) seen)
                   collect (list (car entry) score nil)))
    (append regexp-results module-results)))

(defun nucleo-completion--merge-regexp-and-module-results
    (needle scorable module-results)
  "Return MODULE-RESULTS with regexp-only matches from SCORABLE prepended.
Walks SCORABLE once, skipping candidates already covered by
MODULE-RESULTS and keeping those that match the regexps expanded
from NEEDLE.  Each prepended entry is a triple
\(CANDIDATE BEST-SCORE nil\) compatible with the legacy module
result list format."
  (let ((seen (make-hash-table :test #'equal :size (length module-results)))
        (case-fold-search completion-ignore-case)
        (regexp-groups (nucleo-completion--term-regexp-groups needle))
        (best-score (or (and module-results
                             (nucleo-completion--result-score
                              (car module-results)))
                        1))
        fuzzy-regexp-groups
        regexp-results)
    (dolist (result module-results)
      (puthash (nucleo-completion--result-candidate result) t seen))
    (dolist (candidate scorable)
      (unless (gethash candidate seen)
        (when (nucleo-completion--regexp-match-p
               (if (nucleo-completion--long-candidate-regexp-p candidate)
                   (or fuzzy-regexp-groups
                       (setq fuzzy-regexp-groups
                             (nucleo-completion--term-regexp-groups
                              needle t)))
                 regexp-groups)
               candidate)
          (push (list candidate best-score nil) regexp-results))))
    (nconc (nreverse regexp-results) module-results)))

(defun nucleo-completion--regexp-only-candidates
    (needle scorable module-candidates)
  "Return SCORABLE candidates not in MODULE-CANDIDATES that match NEEDLE regexps.
Walks SCORABLE once.  Long candidates fall back to the fuzzy
regexp set, mirroring `nucleo-completion--regexp-filter-pairs'."
  (let ((seen (make-hash-table :test #'equal
                               :size (max 1 (length module-candidates))))
        (case-fold-search completion-ignore-case)
        (regexp-groups (nucleo-completion--term-regexp-groups needle))
        fuzzy-regexp-groups
        result)
    (dolist (candidate module-candidates)
      (puthash candidate t seen))
    (dolist (candidate scorable)
      (unless (gethash candidate seen)
        (when (nucleo-completion--regexp-match-p
               (if (nucleo-completion--long-candidate-regexp-p candidate)
                   (or fuzzy-regexp-groups
                       (setq fuzzy-regexp-groups
                             (nucleo-completion--term-regexp-groups
                              needle t)))
                 regexp-groups)
               candidate)
          (push candidate result))))
    (nreverse result)))

(defun nucleo-completion--prepend-regexp-only-matches
    (needle scorable bundle)
  "Return BUNDLE with regexp-only matches from SCORABLE prepended.
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
      (list (append extra module-candidates)
            (append (mapcar (lambda (cand) (list cand max-score nil)) extra)
                    top-info)
            (when full-scores
              (append (mapcar (lambda (_) max-score) extra)
                      full-scores))))))

(defun nucleo-completion--exact-word-regexps (needle)
  "Return regexps that match NEEDLE terms as complete words."
  (if (hash-table-p nucleo-completion--exact-word-regexp-cache)
      (let ((cached (gethash needle nucleo-completion--exact-word-regexp-cache
                             :missing)))
        (if (not (eq cached :missing))
            cached
          (let ((regexps (nucleo-completion--exact-word-regexps-1 needle)))
            (puthash needle regexps nucleo-completion--exact-word-regexp-cache)
            regexps)))
    (nucleo-completion--exact-word-regexps-1 needle)))

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

(defun nucleo-completion--score-table (scored)
  "Return a hash table mapping candidates to scores from SCORED."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (entry scored)
      (puthash (car entry) (cdr entry) table))
    table))

(defun nucleo-completion--score-table-from-results (results)
  "Return a hash table mapping candidates from RESULTS to scores.
Avoids materializing the intermediate (CANDIDATE . SCORE) list
that `nucleo-completion--results->scored' would allocate."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (result results)
      (puthash (nucleo-completion--result-candidate result)
               (nucleo-completion--result-score result)
               table))
    table))

(defun nucleo-completion--highlight-regexp (regexp haystack)
  "Highlight REGEXP matches in HAYSTACK."
  (let ((start 0))
    (while (and (<= start (length haystack))
                (string-match regexp haystack start))
      (let ((beg (match-beginning 0))
            (end (match-end 0)))
        (when (< beg end)
          (add-face-text-property beg end
                                  'completions-common-part nil haystack))
        (setq start (if (< beg end) end (1+ start)))))))

(defun nucleo-completion-highlight (needle haystack &optional indices)
  "Highlight destructively the NEEDLE matches in HAYSTACK.
When INDICES is non-nil, highlight those precomputed match
positions instead of recomputing a subsequence match."
  (unless (nucleo-completion--long-candidate-highlight-p haystack)
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
      (unless (nucleo-completion--long-candidate-regexp-p haystack)
        (dolist (term (nucleo-completion--terms needle))
          (dolist (regexp (nucleo-completion--regexp-function-regexps term))
            (nucleo-completion--highlight-regexp regexp haystack))))))
  haystack)

(defun nucleo-completion--highlight-candidate
    (needle haystack &optional score max-score indices)
  "Highlight HAYSTACK against NEEDLE with match and optional score-band faces.
SCORE is compared to MAX-SCORE when both are non-nil."
  (setq haystack (nucleo-completion--apply-score-band
                  needle haystack score max-score))
  (nucleo-completion-highlight needle haystack indices))

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
    (let* ((beforepoint (substring string 0 point))
           (afterpoint (if point (substring string point) ""))
           (bounds (completion-boundaries beforepoint table pred afterpoint))
           (prefix (substring beforepoint 0 (car bounds)))
           (needle (substring beforepoint (car bounds)))
           (module-p (nucleo-completion--module-ready-p))
           (expanded-regexp-p (nucleo-completion--expanded-regexp-p needle))
           (highlight-limit (nucleo-completion--highlight-limit))
           (lazy-hilit-p (bound-and-true-p completion-lazy-hilit))
           (need-full-scores
            (and lazy-hilit-p nucleo-completion-highlight-score-bands))
           (completion-regexp-list
            (if (or expanded-regexp-p module-p)
                completion-regexp-list
              (append
               (apply #'append (nucleo-completion--term-regexp-groups needle))
               completion-regexp-list)))
           (all (if (and (string= prefix "") (stringp (car-safe table))
                         (not (or pred completion-regexp-list
                                  (string= needle ""))))
                    table
                  (all-completions prefix table pred)))
           bundle
           top-info
           full-scores
           max-score
           long-filtered)
      (unless (equal prefix nucleo-completion--current-prefix)
        (setq nucleo-completion--current-prefix prefix
              nucleo-completion--current-result nil))
      (cond
       ((or (null all) (string= needle "")))
       (module-p
        (pcase-let ((`(,scorable ,long)
                     (nucleo-completion--split-scored-candidates all)))
          (setq long-filtered (nucleo-completion--regexp-filter needle long))
          (when scorable
            (setq bundle (nucleo-completion--module-results
                          needle scorable completion-ignore-case
                          highlight-limit need-full-scores)))
          (when (and scorable expanded-regexp-p)
            (setq bundle
                  (nucleo-completion--prepend-regexp-only-matches
                   needle scorable (or bundle (list nil nil nil)))))
          (when bundle
            (setq top-info (nucleo-completion--bundle-top-info bundle)
                  full-scores (nucleo-completion--bundle-full-scores bundle)))
          (setq all (append
                     (and bundle (nucleo-completion--bundle-candidates bundle))
                     long-filtered))))
       (t
        (setq all (nucleo-completion--fallback-filter needle all))))
      (when top-info
        (setq max-score (nucleo-completion--top-info-score (car top-info))))
      (setq nucleo-completion--filtering-p (not (string= needle "")))
      (setq nucleo-completion--current-prefix prefix
            nucleo-completion--current-result
            (if (string= prefix "") all (copy-sequence all)))
      (defvar completion-lazy-hilit-fn)
      (cond
       (lazy-hilit-p
        (let ((info-hash (and top-info
                              (nucleo-completion--top-info-hash top-info)))
              (score-hash (and full-scores
                               (nucleo-completion--full-scores-hash
                                (nucleo-completion--bundle-candidates bundle)
                                full-scores))))
          (setq completion-lazy-hilit-fn
                (lambda (candidate)
                  (let* ((info (and info-hash
                                    (gethash candidate info-hash)))
                         (score (or (and info
                                         (nucleo-completion--top-info-score
                                          info))
                                    (and score-hash
                                         (gethash candidate score-hash))))
                         (indices (and info
                                       (nucleo-completion--top-info-indices
                                        info))))
                    (nucleo-completion--highlight-candidate
                     needle candidate score max-score indices))))))
       ((and (> highlight-limit 0) all)
        (let ((info-hash (and top-info
                              (nucleo-completion--top-info-hash top-info))))
          (cl-loop repeat highlight-limit
                   for x in-ref all
                   for info = (and info-hash (gethash x info-hash))
                   for score = (and info
                                    (nucleo-completion--top-info-score info))
                   for indices = (and info
                                      (nucleo-completion--top-info-indices
                                       info))
                   do (setf x (nucleo-completion--highlight-candidate
                               needle (copy-sequence x) score max-score
                               indices))))))
      (and all (if (string= prefix "") all (nconc all (length prefix)))))))

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
               '(nucleo completion-flex-try-completion
                        nucleo-completion-all-completions
                        "Fuzzy completion backed by nucleo-matcher."))
  (put 'nucleo 'completion--adjust-metadata
       #'nucleo-completion-adjust-metadata))

(provide 'nucleo-completion)
;;; nucleo-completion.el ends here
