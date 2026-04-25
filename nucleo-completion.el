;;; nucleo-completion.el --- Nucleo-backed completion style  -*- lexical-binding: t -*-

;; Copyright (C) 2026

;; Author: Nobu
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: matching, convenience
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
(declare-function nucleo-completion-score "nucleo-completion-module")
(declare-function nucleo-completion-scored-filter "nucleo-completion-module")

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

(defvar nucleo-completion--filtering-p nil)

(defconst nucleo-completion--directory
  (file-name-directory (or load-file-name byte-compile-current-file buffer-file-name))
  "Directory containing nucleo-completion.el.")

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
  "Filter CANDIDATES with fuzzy and configured regexp matchers."
  (let ((case-fold-search completion-ignore-case)
        (regexp-groups (nucleo-completion--term-regexp-groups needle)))
    (cl-loop for candidate in candidates
             when (nucleo-completion--regexp-match-p regexp-groups candidate)
             collect candidate)))

(defun nucleo-completion--fallback-filter (needle candidates)
  "Filter CANDIDATES against NEEDLE without Rust module sorting."
  (nucleo-completion--regexp-filter needle candidates))

(defun nucleo-completion--sort-with-module (needle candidates ignore-case)
  "Sort CANDIDATES with the Rust module without dropping regexp-only matches."
  (let* ((scored (nucleo-completion--module-filter needle candidates ignore-case))
         (seen (make-hash-table :test #'equal)))
    (dolist (candidate scored)
      (puthash candidate t seen))
    (nconc scored
           (cl-loop for candidate in candidates
                    unless (gethash candidate seen)
                    collect candidate))))

(defun nucleo-completion--scored-filter (needle candidates ignore-case)
  "Return matching CANDIDATES as (CANDIDATE . SCORE) pairs."
  (if (fboundp 'nucleo-completion-scored-filter)
      (nucleo-completion-scored-filter needle candidates ignore-case)
    (cl-loop for candidate in candidates
             for score = (nucleo-completion-score needle candidate ignore-case)
             when score
             collect (cons candidate score))))

(defun nucleo-completion--string-lessp (a b ignore-case)
  "Return non-nil when A should sort before B alphabetically."
  (if ignore-case
      (string-lessp (downcase a) (downcase b))
    (string-lessp a b)))

(defun nucleo-completion--scored-entry-lessp (ignore-case a b)
  "Return non-nil when scored entry A should sort before B."
  (let ((a-candidate (car a))
        (a-score (cdr a))
        (b-candidate (car b))
        (b-score (cdr b)))
    (cond
     ((> a-score b-score) t)
     ((< a-score b-score) nil)
     ((and nucleo-completion-sort-ties-by-length
           (/= (length a-candidate) (length b-candidate)))
      (< (length a-candidate) (length b-candidate)))
     (nucleo-completion-sort-ties-alphabetically
      (nucleo-completion--string-lessp a-candidate b-candidate ignore-case))
     (t nil))))

(defun nucleo-completion--module-filter (needle candidates ignore-case)
  "Filter and sort CANDIDATES with the Rust module."
  (if (or nucleo-completion-sort-ties-by-length
          nucleo-completion-sort-ties-alphabetically)
      (mapcar #'car
              (cl-stable-sort (nucleo-completion--scored-filter
                               needle candidates ignore-case)
                              (apply-partially
                               #'nucleo-completion--scored-entry-lessp
                               ignore-case)))
    (nucleo-completion-filter needle candidates ignore-case)))

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

;;;###autoload
(defun nucleo-completion-all-completions (string table &optional pred point)
  "Get Nucleo completions of STRING in TABLE.
See `completion-all-completions' for the semantics of PRED and POINT."
  (let* ((beforepoint (substring string 0 point))
         (afterpoint (if point (substring string point) ""))
         (bounds (completion-boundaries beforepoint table pred afterpoint))
         (prefix (substring beforepoint 0 (car bounds)))
         (needle (substring beforepoint (car bounds)))
         (module-p (fboundp 'nucleo-completion-filter))
         (expanded-regexp-p (nucleo-completion--expanded-regexp-p needle))
         (completion-regexp-list
          (if (or expanded-regexp-p module-p)
              completion-regexp-list
            (append
             (apply #'append (nucleo-completion--term-regexp-groups needle))
             completion-regexp-list)))
         (all (if (and (string= prefix "") (stringp (car-safe table))
                       (not (or pred completion-regexp-list (string= needle ""))))
                  table
                (all-completions prefix table pred))))
    (cond
     ((or (null all) (string= needle "")))
     ((and module-p expanded-regexp-p)
      (setq all (nucleo-completion--sort-with-module
                 needle
                 (nucleo-completion--regexp-filter needle all)
                 completion-ignore-case)))
     (module-p
      (setq all (nucleo-completion--module-filter
                 needle all completion-ignore-case)))
     (t
      (setq all (nucleo-completion--fallback-filter needle all))))
    (setq nucleo-completion--filtering-p (not (string= needle "")))
    (defvar completion-lazy-hilit-fn)
    (if (bound-and-true-p completion-lazy-hilit)
        (setq completion-lazy-hilit-fn
              (apply-partially #'nucleo-completion-highlight needle))
      (cl-loop repeat nucleo-completion-max-highlighted-completions
               for x in-ref all
               do (setf x (nucleo-completion-highlight needle (copy-sequence x)))))
    (and all (if (string= prefix "") all (nconc all (length prefix))))))

;;;###autoload
(defun nucleo-completion--adjust-metadata (metadata)
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
       #'nucleo-completion--adjust-metadata))

(provide 'nucleo-completion)
;;; nucleo-completion.el ends here
