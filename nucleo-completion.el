;;; nucleo-completion.el --- Nucleo-backed completion style  -*- lexical-binding: t -*-

;; Copyright (C) 2026

;; Author: Nobu <https://github.com/kn66>
;; Assisted-by: OpenAI Codex
;; Version: 0.1.3
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

(declare-function nucleo-completion-filter "nucleo-completion-module")
(declare-function nucleo-completion-scored-filter "nucleo-completion-module")
(declare-function nucleo-completion-sorted-filter "nucleo-completion-module")
(declare-function nucleo-completion-sorted-scored-filter "nucleo-completion-module")

(defgroup nucleo-completion nil
  "Nucleo-backed fuzzy completion style."
  :group 'minibuffer)

(defcustom nucleo-completion-max-highlighted-completions 25
  "Number of top-ranking completions to highlight eagerly.
Large values can decrease performance."
  :type 'integer)

(defcustom nucleo-completion-regexp-functions nil
  "Functions used to expand each search term to extra regexps.
Each function is called with one whitespace-separated search term.
It may return a regexp string, a list of regexp strings, or nil.

The built-in fuzzy subsequence matcher is always used.  Regexps
from this option are ORed with it for each term, while terms are
ANDed together."
  :type '(repeat function))

(defcustom nucleo-completion-sort-ties-by-length nil
  "Whether to sort equal-scoring Nucleo matches by candidate length.
When non-nil, shorter candidates come first.  This only affects
candidates with the same Nucleo score."
  :type 'boolean)

(defcustom nucleo-completion-sort-ties-alphabetically nil
  "Whether to sort equal-scoring Nucleo matches alphabetically.
When `nucleo-completion-sort-ties-by-length' is also non-nil,
alphabetical order is used after comparing length.  This only
affects candidates with the same Nucleo score."
  :type 'boolean)

(defcustom nucleo-completion-highlight-score-bands nil
  "Whether to highlight high- and low-scoring completion candidates.
When non-nil, candidates with exact word matches or scores close to
the best score use `nucleo-completion-high-score-face'.  Other
scored candidates use `nucleo-completion-low-score-face'."
  :type 'boolean)

(defcustom nucleo-completion-use-cache t
  "Whether to reuse the previous narrowed candidate set.
When non-nil, `nucleo-completion-all-completions' may avoid
calling `all-completions' on the original completion table when the
current input extends the previous input in the same completion
context.  This is disabled automatically when regexp expanders are
active."
  :type 'boolean)

(defcustom nucleo-completion-high-score-ratio 0.85
  "Minimum ratio to the best score for high-score candidate highlighting.
For example, 0.85 means candidates whose score is at least 85% of
the best score are highlighted with
`nucleo-completion-high-score-face'.  Exact word matches are always
treated as high-scoring candidates."
  :type 'number)

(defcustom nucleo-completion-high-score-emphasis '(bold underline)
  "Additional emphasis faces for high-score candidate highlighting.
The value is a list containing `bold', `underline', both, or nil.
This option only affects high-score candidates when
`nucleo-completion-highlight-score-bands' is non-nil."
  :type '(set (const :tag "Bold" bold)
              (const :tag "Underline" underline)))

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

(defvar-local nucleo-completion--candidate-cache nil
  "Cache for reusing narrowed completion candidates in one buffer.")

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

(defun nucleo-completion--module-candidates ()
  "Return candidate paths for the Rust dynamic module."
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
                          (expand-file-name "target/debug" nucleo-completion--directory))))))))

(defun nucleo-completion--load-module ()
  "Load the Rust dynamic module when it is available."
  (unless (featurep 'nucleo-completion-module)
    (catch 'loaded
      (dolist (file (nucleo-completion--module-candidates))
        (when (and file (file-readable-p file))
          (condition-case nil
              (progn
                (module-load file)
                (throw 'loaded t))
            (error nil)))))))

(nucleo-completion--load-module)

(defun nucleo-completion--terms (pattern)
  "Split PATTERN into non-empty whitespace-separated terms."
  (split-string pattern "[[:space:]]+" t))

(defun nucleo-completion--subsequence-regexp (needle)
  "Return a fuzzy subsequence regexp for NEEDLE."
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
  (cl-loop for function in nucleo-completion-regexp-functions
           for value = (when (functionp function)
                         (ignore-errors (funcall function term)))
           append (cond
                   ((nucleo-completion--valid-regexp-p value)
                    (list value))
                   ((listp value)
                    (cl-remove-if-not #'nucleo-completion--valid-regexp-p value)))))

(defun nucleo-completion--term-regexps (term)
  "Return regexps that may match TERM."
  (cons (concat "\\`" (nucleo-completion--subsequence-regexp term))
        (nucleo-completion--regexp-function-regexps term)))

(defun nucleo-completion--term-regexp-groups (needle)
  "Return regexp groups for NEEDLE.
Each group corresponds to one term.  A candidate must match at
least one regexp from every group."
  (mapcar #'nucleo-completion--term-regexps
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
        (regexp-groups (nucleo-completion--term-regexp-groups needle)))
    (cl-loop for candidate in candidates
             when (nucleo-completion--regexp-match-p regexp-groups candidate)
             collect (cons candidate nil))))

(defun nucleo-completion--fallback-filter (needle candidates)
  "Filter CANDIDATES against NEEDLE without scoring or sorting."
  (nucleo-completion--regexp-filter needle candidates))

(defun nucleo-completion--sort-with-module
    (needle candidates ignore-case &optional regexp-pairs)
  "Sort CANDIDATES against NEEDLE with the Rust module.
Honor IGNORE-CASE and preserve optional REGEXP-PAIRS regexp-only matches."
  (mapcar #'car
          (nucleo-completion--sort-scored-with-module
           needle candidates ignore-case regexp-pairs)))

(defun nucleo-completion--sort-scored-with-module
    (needle candidates ignore-case &optional regexp-pairs)
  "Return scored CANDIDATES for NEEDLE sorted with the Rust module.
Honor IGNORE-CASE.

Keep regexp-only matches by merging REGEXP-PAIRS entries that were
not returned by the module."
  (car (nucleo-completion--sort-and-module-scored-with-module
        needle candidates ignore-case regexp-pairs)))

(defun nucleo-completion--sort-and-module-scored-with-module
    (needle candidates ignore-case &optional regexp-pairs)
  "Return sorted scores for NEEDLE and CANDIDATES.
The car is the full sorted scored list, including promoted
regexp-only matches from REGEXP-PAIRS.  The cdr is the scored list
used for visual score-band highlighting.  Honor IGNORE-CASE."
  (let ((module-scored
         (nucleo-completion-scored-filter needle candidates ignore-case))
        (regexp-pairs (or regexp-pairs
                          (mapcar (lambda (candidate)
                                    (cons candidate nil))
                                  candidates)))
        (seen (make-hash-table :test #'equal))
        regexp-scored)
    (dolist (entry module-scored)
      (puthash (car entry) t seen))
    (setq regexp-scored
          (cl-loop with score = (or (cdar module-scored) 1)
                   for entry in regexp-pairs
                   unless (gethash (car entry) seen)
                   collect (cons (car entry) score)))
    (let ((scored (append regexp-scored module-scored)))
      (cons scored scored))))

(defun nucleo-completion--module-filter (needle candidates ignore-case)
  "Filter and sort CANDIDATES against NEEDLE with the Rust module.
Honor IGNORE-CASE."
  (if (or nucleo-completion-sort-ties-by-length
          nucleo-completion-sort-ties-alphabetically)
      (nucleo-completion-sorted-filter
       needle candidates ignore-case
       nucleo-completion-sort-ties-by-length
       nucleo-completion-sort-ties-alphabetically)
    (nucleo-completion-filter needle candidates ignore-case)))

(defun nucleo-completion--module-filter-with-scores
    (needle candidates ignore-case)
  "Filter CANDIDATES against NEEDLE with the Rust module and keep scores.
Honor IGNORE-CASE."
  (if (or nucleo-completion-sort-ties-by-length
          nucleo-completion-sort-ties-alphabetically)
      (nucleo-completion-sorted-scored-filter
       needle candidates ignore-case
       nucleo-completion-sort-ties-by-length
       nucleo-completion-sort-ties-alphabetically)
    (nucleo-completion-scored-filter needle candidates ignore-case)))

(defun nucleo-completion--module-ready-p ()
  "Return non-nil when the current Rust module provides the batch API."
  (and (fboundp 'nucleo-completion-filter)
       (fboundp 'nucleo-completion-scored-filter)
       (fboundp 'nucleo-completion-sorted-filter)
       (fboundp 'nucleo-completion-sorted-scored-filter)))

(defun nucleo-completion--cache-reusable-p
    (cache table pred prefix needle ignore-case expanded-regexp-p)
  "Return non-nil when CACHE can provide candidates for this request.
TABLE, PRED, PREFIX, NEEDLE, IGNORE-CASE, and EXPANDED-REGEXP-P
describe the current completion request."
  (and nucleo-completion-use-cache
       cache
       (not expanded-regexp-p)
       (null nucleo-completion-regexp-functions)
       (null completion-regexp-list)
       (eq table (plist-get cache :table))
       (eq pred (plist-get cache :pred))
       (equal prefix (plist-get cache :prefix))
       (eq ignore-case (plist-get cache :ignore-case))
       (string-prefix-p (plist-get cache :needle) needle)
       (not (string= (plist-get cache :needle) ""))))

(defun nucleo-completion--cache-candidates
    (table pred prefix needle ignore-case all)
  "Remember ALL candidates for this completion context.
TABLE, PRED, PREFIX, NEEDLE, and IGNORE-CASE identify the context."
  (when (and nucleo-completion-use-cache
             (null nucleo-completion-regexp-functions)
             (null completion-regexp-list)
             (not (string= needle "")))
    (setq nucleo-completion--candidate-cache
          (list :table table
                :pred pred
                :prefix prefix
                :needle needle
                :ignore-case ignore-case
                :all (copy-sequence all)))))

(defun nucleo-completion--cached-candidates
    (table pred prefix needle ignore-case expanded-regexp-p)
  "Return cached candidates suitable for filtering NEEDLE, or nil.
TABLE, PRED, PREFIX, IGNORE-CASE, and EXPANDED-REGEXP-P describe
the current completion request."
  (when (nucleo-completion--cache-reusable-p
         nucleo-completion--candidate-cache table pred prefix needle
         ignore-case expanded-regexp-p)
    (copy-sequence (plist-get nucleo-completion--candidate-cache :all))))

(defun nucleo-completion--clear-cache ()
  "Clear the narrowed candidate cache."
  (setq nucleo-completion--candidate-cache nil))

(defun nucleo-completion--exact-word-match-p (needle candidate)
  "Return non-nil when every NEEDLE term occurs as a word in CANDIDATE."
  (let ((case-fold-search completion-ignore-case))
    (cl-every (lambda (term)
                (string-match-p
                 (concat "\\(?:\\`\\|[^[:alnum:]_]\\)"
                         (regexp-quote term)
                         "\\(?:\\'\\|[^[:alnum:]_]\\)")
                 candidate))
              (nucleo-completion--terms needle))))

(defun nucleo-completion--high-score-p
    (needle candidate score max-score)
  "Return non-nil if CANDIDATE belongs to NEEDLE's high score band.
SCORE is compared to MAX-SCORE."
  (or (nucleo-completion--exact-word-match-p needle candidate)
      (and max-score
           (> max-score 0)
           (>= score (* max-score nucleo-completion-high-score-ratio)))))

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

(defun nucleo-completion-highlight (needle haystack)
  "Highlight destructively the NEEDLE matches in HAYSTACK."
  (let ((case-fold-search completion-ignore-case))
    (dolist (term (nucleo-completion--terms needle))
      (let ((start 0))
        (cl-loop for ch across term
                 for match = (string-match (regexp-quote (char-to-string ch))
                                           haystack start)
                 while match
                 do (add-face-text-property match (1+ match)
                                            'completions-common-part nil haystack)
                 (setq start (1+ match))))
      (dolist (regexp (nucleo-completion--regexp-function-regexps term))
        (nucleo-completion--highlight-regexp regexp haystack))))
  haystack)

(defun nucleo-completion--highlight-candidate
    (needle haystack &optional score max-score)
  "Highlight HAYSTACK against NEEDLE with match and optional score-band faces.
SCORE is compared to MAX-SCORE when both are non-nil."
  (setq haystack (nucleo-completion--apply-score-band
                  needle haystack score max-score))
  (nucleo-completion-highlight needle haystack))

(defun nucleo-completion--all-completions-1 (string table &optional pred point)
  "Get Nucleo completions of STRING in TABLE.
See `completion-all-completions' for the semantics of PRED and POINT."
  (let* ((beforepoint (substring string 0 point))
         (afterpoint (if point (substring string point) ""))
         (bounds (completion-boundaries beforepoint table pred afterpoint))
         (prefix (substring beforepoint 0 (car bounds)))
         (needle (substring beforepoint (car bounds)))
         (module-p (nucleo-completion--module-ready-p))
         (expanded-regexp-p (nucleo-completion--expanded-regexp-p needle))
         (completion-regexp-list
          (if (or expanded-regexp-p module-p)
              completion-regexp-list
            (append
             (apply #'append (nucleo-completion--term-regexp-groups needle))
             completion-regexp-list)))
         (cached-all
          (nucleo-completion--cached-candidates
           table pred prefix needle completion-ignore-case expanded-regexp-p))
         (all (or cached-all
                  (if (and (string= prefix "") (stringp (car-safe table))
                           (not (or pred completion-regexp-list
                                    (string= needle ""))))
                      table
                    (all-completions prefix table pred))))
         scored
         visual-scored
         score-table
         max-score)
    (setq nucleo-completion--current-prefix prefix
          nucleo-completion--current-result nil)
    (when all
      (setq nucleo-completion--current-result (copy-sequence all)))
    (cond
     ((or (null all) (string= needle "")))
     ((and module-p expanded-regexp-p)
      (let ((regexp-pairs
             (nucleo-completion--regexp-filter-pairs needle all)))
        (if nucleo-completion-highlight-score-bands
            (let ((sorted-and-module-scored
                   (nucleo-completion--sort-and-module-scored-with-module
                    needle
                    (mapcar #'car regexp-pairs)
                    completion-ignore-case
                    regexp-pairs)))
              (setq scored (car sorted-and-module-scored)
                    visual-scored (cdr sorted-and-module-scored)
                    all (mapcar #'car scored)))
          (setq all (nucleo-completion--sort-with-module
                     needle
                     (mapcar #'car regexp-pairs)
                     completion-ignore-case
                     regexp-pairs)))))
     (module-p
      (if nucleo-completion-highlight-score-bands
          (setq scored (nucleo-completion--module-filter-with-scores
                        needle all completion-ignore-case)
                visual-scored scored
                all (mapcar #'car scored))
        (setq all (nucleo-completion--module-filter
                   needle all completion-ignore-case))))
     (t
      (setq all (nucleo-completion--fallback-filter needle all))))
    (when (and all (not expanded-regexp-p))
      (nucleo-completion--cache-candidates
       table pred prefix needle completion-ignore-case all))
    (when visual-scored
      (setq max-score (cdar visual-scored)
            score-table (nucleo-completion--score-table visual-scored)))
    (setq nucleo-completion--filtering-p (not (string= needle "")))
    (setq nucleo-completion--current-result (copy-sequence all))
    (defvar completion-lazy-hilit-fn)
    (if (bound-and-true-p completion-lazy-hilit)
        (setq completion-lazy-hilit-fn
              (lambda (candidate)
                (let ((score (and score-table
                                  (gethash (substring-no-properties candidate)
                                           score-table))))
                  (nucleo-completion--highlight-candidate
                   needle candidate score max-score))))
      (cl-loop repeat nucleo-completion-max-highlighted-completions
               for x in-ref all
               for score = (and score-table (gethash x score-table))
               do (setf x (nucleo-completion--highlight-candidate
                            needle (copy-sequence x) score max-score))))
    (and all (if (string= prefix "") all (nconc all (length prefix))))))

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
