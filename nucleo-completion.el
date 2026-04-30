;;; nucleo-completion.el --- Nucleo-backed completion style  -*- lexical-binding: t -*-

;; Copyright (C) 2026

;; Author: Nobu <https://github.com/kn66>
;; Assisted-by: OpenAI Codex
;; Version: 0.1.4
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

(defcustom nucleo-completion-long-candidate-threshold 4096
  "Maximum candidate length to score with the Rust module.
Candidates longer than this number of characters are filtered but
not scored.  Long matching candidates are appended after scored
matches in their original order.  Set this to nil to score every
candidate regardless of length."
  :type '(choice (const :tag "Score every candidate" nil)
                 (natnum :tag "Maximum scored candidate length"))
  :group 'nucleo-completion)

(defcustom nucleo-completion-long-candidate-regexp-threshold 4096
  "Maximum candidate length to match with regexp expanders.
Candidates longer than this number of characters only use the
built-in fuzzy subsequence matcher; regexps from
`nucleo-completion-regexp-functions' are skipped during filtering
and highlighting.  Set this to nil to use regexp expanders for
every candidate regardless of length."
  :type '(choice (const :tag "Use regexp expanders for every candidate" nil)
                 (natnum :tag "Maximum regexp-expanded candidate length"))
  :group 'nucleo-completion)

(defcustom nucleo-completion-long-candidate-highlight-threshold 4096
  "Maximum candidate length to highlight.
Candidates longer than this number of characters are returned
without match highlighting.  Set this to nil to highlight every
candidate regardless of length."
  :type '(choice (const :tag "Highlight every candidate" nil)
                 (natnum :tag "Maximum highlighted candidate length"))
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
  "Return STRING without text properties."
  (if (stringp string)
      (substring-no-properties string)
    string))

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
  "Return sanitized long-candidate regexp threshold, or nil."
  (when (and (integerp nucleo-completion-long-candidate-regexp-threshold)
             (>= nucleo-completion-long-candidate-regexp-threshold 0))
    nucleo-completion-long-candidate-regexp-threshold))

(defun nucleo-completion--long-candidate-regexp-p (candidate)
  "Return non-nil when CANDIDATE should skip regexp expanders."
  (let ((threshold (nucleo-completion--long-candidate-regexp-threshold)))
    (and threshold
         (> (length candidate) threshold))))

(defun nucleo-completion--long-candidate-highlight-threshold ()
  "Return sanitized long-candidate highlight threshold, or nil."
  (when (and (integerp nucleo-completion-long-candidate-highlight-threshold)
             (>= nucleo-completion-long-candidate-highlight-threshold 0))
    nucleo-completion-long-candidate-highlight-threshold))

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
module.  LONG contains candidates that should only be filtered."
  (let (scorable long)
    (dolist (candidate candidates)
      (if (nucleo-completion--long-candidate-p candidate)
          (push candidate long)
        (push candidate scorable)))
    (list (nreverse scorable) (nreverse long))))

(defun nucleo-completion--fallback-filter (needle candidates)
  "Filter CANDIDATES against NEEDLE without scoring or sorting."
  (nucleo-completion--regexp-filter needle candidates))

(defun nucleo-completion--module-filter (needle candidates ignore-case)
  "Filter and sort CANDIDATES against NEEDLE with the Rust module.
Honor IGNORE-CASE."
  (mapcar #'nucleo-completion--result-candidate
          (nucleo-completion--module-results needle candidates ignore-case 0)))

(defun nucleo-completion--module-filter-with-scores
    (needle candidates ignore-case)
  "Filter CANDIDATES against NEEDLE with the Rust module and keep scores.
Honor IGNORE-CASE."
  (nucleo-completion--results->scored
   (nucleo-completion--module-results needle candidates ignore-case 0)))

(defun nucleo-completion--module-ready-p ()
  "Return non-nil when the current Rust module provides the batch API."
  (fboundp 'nucleo-completion-candidates))

(defun nucleo-completion--module-results
    (needle candidates ignore-case highlight-limit)
  "Return module result entries for NEEDLE and CANDIDATES.
Each entry has the form (CANDIDATE SCORE INDICES).  Honor
IGNORE-CASE and ask the module for INDICES for at most
HIGHLIGHT-LIMIT top-ranking candidates."
  (nucleo-completion-candidates
   needle candidates ignore-case
   nucleo-completion-sort-ties-by-length
   nucleo-completion-sort-ties-alphabetically
   highlight-limit))

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
  "Return hash table mapping candidates in RESULTS to precomputed indices."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (result results)
      (when (nucleo-completion--result-indices result)
        (puthash (nucleo-completion--string-key
                  (nucleo-completion--result-candidate result))
                 (nucleo-completion--result-indices result)
                 table)))
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
      (puthash (nucleo-completion--string-key
                (nucleo-completion--result-candidate result))
               t seen))
    (setq regexp-results
          (cl-loop with score = (or (nucleo-completion--result-score
                                     (car module-results))
                                    1)
                   for entry in regexp-pairs
                   unless (gethash (nucleo-completion--string-key (car entry))
                                   seen)
                   collect (list (car entry) score nil)))
    (append regexp-results module-results)))

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
      (puthash (nucleo-completion--string-key (car entry)) (cdr entry) table))
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
  (let ((nucleo-completion--regexp-cache (make-hash-table :test #'equal))
        (nucleo-completion--terms-cache (make-hash-table :test #'equal))
        (nucleo-completion--subsequence-regexp-cache
         (make-hash-table :test #'equal))
        (nucleo-completion--exact-word-regexp-cache
         (make-hash-table :test #'equal)))
    (let* ((beforepoint (substring string 0 point))
           (afterpoint (if point (substring string point) ""))
           (bounds (completion-boundaries beforepoint table pred afterpoint))
           (prefix (substring beforepoint 0 (car bounds)))
           (needle (substring beforepoint (car bounds)))
           (module-p (nucleo-completion--module-ready-p))
           (expanded-regexp-p (nucleo-completion--expanded-regexp-p needle))
           (highlight-limit (nucleo-completion--highlight-limit))
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
           visual-scored
           module-results
           score-table
           indices-table
           max-score
           long-filtered)
      (unless (equal prefix nucleo-completion--current-prefix)
        (setq nucleo-completion--current-prefix prefix
              nucleo-completion--current-result nil))
      (cond
       ((or (null all) (string= needle "")))
       ((and module-p expanded-regexp-p)
        (pcase-let ((`(,scorable ,long)
                     (nucleo-completion--split-scored-candidates all)))
          (setq module-results
                (when scorable
                  (nucleo-completion--module-results
                   needle scorable completion-ignore-case highlight-limit))
                long-filtered
                (nucleo-completion--regexp-filter needle long))
          (let ((regexp-pairs
                 (nucleo-completion--regexp-filter-pairs-excluding
                  needle scorable
                  (nucleo-completion--results->candidate-table
                   module-results))))
            (setq module-results
                  (nucleo-completion--merge-regexp-results regexp-pairs
                                                           module-results)))
          (setq visual-scored
                (nucleo-completion--results->scored module-results)
                all (append
                     (mapcar #'nucleo-completion--result-candidate
                             module-results)
                     long-filtered))))
       (module-p
        (pcase-let ((`(,scorable ,long)
                     (nucleo-completion--split-scored-candidates all)))
          (setq module-results
                (when scorable
                  (nucleo-completion--module-results
                   needle scorable completion-ignore-case
                   highlight-limit))
                long-filtered
                (nucleo-completion--regexp-filter needle long)
                visual-scored
                (nucleo-completion--results->scored module-results)
                all (append
                     (mapcar #'nucleo-completion--result-candidate
                             module-results)
                     long-filtered))))
       (t
        (setq all (nucleo-completion--fallback-filter needle all))))
      (when visual-scored
        (setq max-score (cdar visual-scored)))
      (setq nucleo-completion--filtering-p (not (string= needle "")))
      (setq nucleo-completion--current-prefix prefix
            nucleo-completion--current-result (copy-sequence all))
      (defvar completion-lazy-hilit-fn)
      (if (bound-and-true-p completion-lazy-hilit)
          (progn
            (when visual-scored
              (setq score-table (nucleo-completion--score-table visual-scored)
                    indices-table
                    (or indices-table
                        (nucleo-completion--results->indices-table module-results))))
            (setq completion-lazy-hilit-fn
                  (lambda (candidate)
                    (let* ((key (substring-no-properties candidate))
                           (score (and score-table (gethash key score-table)))
                           (indices (and indices-table
                                         (gethash key indices-table))))
                      (nucleo-completion--highlight-candidate
                       needle candidate score max-score indices)))))
        (when (and visual-scored (> highlight-limit 0))
          (setq score-table (nucleo-completion--score-table visual-scored)
                indices-table
                (nucleo-completion--results->indices-table module-results)))
        (cl-loop repeat highlight-limit
                 for x in-ref all
                 for score = (and score-table (gethash x score-table))
                 for indices = (and indices-table (gethash x indices-table))
                 do (setf x (nucleo-completion--highlight-candidate
                             needle (copy-sequence x) score max-score
                             indices))))
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
