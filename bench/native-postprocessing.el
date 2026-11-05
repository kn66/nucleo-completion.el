;;; native-postprocessing.el --- Compare native history and merging -*- lexical-binding: t -*-

;; Run after `cargo build --release':
;; emacs -Q --batch -L . -l bench/native-postprocessing.el

(setq load-prefer-newer t)
(require 'benchmark)
(require 'cl-lib)
(require 'nucleo-completion)

;; Installed modules take precedence in normal use.  Benchmarks must compare
;; the working tree's release binary, even when its version is unchanged.
(module-load (expand-file-name
              (nucleo-completion--rust-library-name)
              (expand-file-name "../target/release"
                                (file-name-directory load-file-name))))

(defvar nucleo-benchmark-iterations 5)
(defvar nucleo-benchmark-rounds 3)
(defvar nucleo-benchmark-candidate-counts '(10000 100000))
(defvar corfu-history nil)
(defvar corfu-history-mode nil)

(defun nucleo-benchmark--candidates (count)
  "Return COUNT candidates with fuzzy and language-only matches."
  (cl-loop for index below count
           collect (format "%s-%06d.txt"
                           (pcase (% index 10)
                             (0 "kensaku")
                             (1 "検索結果")
                             (_ "project-file"))
                           index)))

(defun nucleo-benchmark--measure (candidates legacy)
  "Time completion of CANDIDATES, using the older path when LEGACY."
  (let ((native-function
         (symbol-function 'nucleo-completion--native-completion-results)))
    (cl-letf (((symbol-function 'nucleo-completion--native-completion-results)
               (if legacy
                   (lambda (&rest _) 'nucleo-completion-unsupported-regexp)
                 native-function)))
      (nucleo-completion-all-completions "kensaku" candidates)
      (garbage-collect)
      (let ((result
             (benchmark-call
              (lambda ()
                (nucleo-completion-all-completions "kensaku" candidates))
              nucleo-benchmark-iterations)))
        (list (* 1000 (/ (car result) nucleo-benchmark-iterations))
              (cadr result))))))

(unless (and (nucleo-completion--module-ready-p)
             (fboundp 'nucleo-completion-candidates-with-options))
  (error "Build a release module with the native options API first"))

(let ((gc-cons-threshold (* 128 1024 1024))
      (completion-ignore-case nil)
      (completion-lazy-hilit nil)
      (nucleo-completion-highlight-score-bands nil)
      (nucleo-completion-regexp-functions nil)
      (nucleo-completion-sort-ties-by-length nil)
      (nucleo-completion-sort-ties-alphabetically nil)
      (nucleo-completion-regexp-only-match-priority 'non-ascii)
      (corfu-history-mode t))
  (princ (format "Emacs %s; median of %d rounds x %d completions; ms/completion\n"
                 emacs-version nucleo-benchmark-rounds
                 nucleo-benchmark-iterations))
  (dolist (count nucleo-benchmark-candidate-counts)
    (let ((candidates (nucleo-benchmark--candidates count)))
      (dolist (workload '(history language combined))
        (let* ((nucleo-completion-languages
                (unless (eq workload 'history) '(japanese)))
               (nucleo-completion-sort-ties-by-history
                (not (eq workload 'language)))
               (corfu-history (cl-loop for index below 100
                                       collect (nth (* index 10) candidates)))
               legacy-times native-times (gc-count 0))
          (when (and nucleo-completion-sort-ties-by-history
                     (null (nucleo-completion--history-rank-table "")))
            (error "History workload requires active Corfu history"))
          ;; Alternate measurement order to reduce systematic warmup bias.
          (dotimes (round nucleo-benchmark-rounds)
            (dolist (legacy (if (cl-evenp round) '(t nil) '(nil t)))
              (pcase-let ((`(,elapsed ,collections)
                            (nucleo-benchmark--measure candidates legacy)))
                (if legacy (push elapsed legacy-times)
                  (push elapsed native-times))
                (cl-incf gc-count collections))))
          (princ
           (format "%d candidates; %s; legacy %.2f; native %.2f; %d GC\n"
                   count workload
                   (nth (/ nucleo-benchmark-rounds 2) (sort legacy-times #'<))
                   (nth (/ nucleo-benchmark-rounds 2) (sort native-times #'<))
                   gc-count)))))))

;;; native-postprocessing.el ends here
