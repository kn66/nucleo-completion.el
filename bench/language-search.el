;;; language-search.el --- Compare native language scans -*- lexical-binding: t; -*-

;; Run from the repository root after `cargo build --release':
;; emacs -Q --batch -L . -l bench/language-search.el
;; The legacy mode disables only the new entry point in this batch process.

(setq load-prefer-newer t)
(require 'benchmark)
(require 'cl-lib)
(require 'nucleo-completion)

;; Normal loading prefers an installed release over the development build.
(module-load (expand-file-name
              (nucleo-completion--rust-library-name)
              (expand-file-name "../target/release"
                                (file-name-directory load-file-name))))

(defvar nucleo-benchmark-iterations 5)
(defvar nucleo-benchmark-candidate-counts '(10000 100000))

(defun nucleo-benchmark--candidates (count)
  "Return COUNT mixed ASCII, Japanese and Chinese completion candidates."
  (cl-loop for index below count
           collect (format "%s-%06d.txt"
                           (pcase (% index 20)
                             ((or 0 1) "kensaku")
                             (2 "検索結果")
                             (3 "中国文件")
                             (_ "project-file"))
                           index)))

(defun nucleo-benchmark--search (query candidates)
  "Measure QUERY against CANDIDATES after warming language data and regexps."
  (nucleo-completion-all-completions query candidates)
  (garbage-collect)
  (let ((result (benchmark-call
                 (lambda () (nucleo-completion-all-completions query candidates))
                 nucleo-benchmark-iterations)))
    (princ (format "%.2f ms, %d GC"
                   (* 1000 (/ (car result) nucleo-benchmark-iterations))
                   (cadr result)))))

(unless (nucleo-completion--module-ready-p)
  (error "Build the release module before running this benchmark"))

(let ((gc-cons-threshold (* 128 1024 1024))
      (completion-ignore-case nil)
      (completion-lazy-hilit nil)
      (nucleo-completion-highlight-score-bands nil)
      (native-function (and (fboundp 'nucleo-completion-candidates-with-regexp-groups)
                            (symbol-function 'nucleo-completion-candidates-with-regexp-groups))))
  (princ (format "Emacs %s; %d warm iterations; time per completion\n"
                 emacs-version nucleo-benchmark-iterations))
  (dolist (count nucleo-benchmark-candidate-counts)
    (let ((candidates (nucleo-benchmark--candidates count)))
      (dolist (query '("kensaku" "beijing"))
        (dolist (languages '(nil (japanese) (chinese)))
          (let ((nucleo-completion-languages languages))
            (princ (format "%d candidates; %s; %S; legacy: " count query languages))
            (unwind-protect
                (progn
                  ;; Exercise the original Lisp regexp scan, including when
                  ;; the newer merged-result entry point is available.
                  (cl-letf (((symbol-function 'nucleo-completion--native-completion-results)
                             (lambda (&rest _)
                               'nucleo-completion-unsupported-regexp)))
                    (fmakunbound 'nucleo-completion-candidates-with-regexp-groups)
                    (nucleo-benchmark--search query candidates)))
              (when native-function
                (fset 'nucleo-completion-candidates-with-regexp-groups native-function)))
            (when native-function
              (princ "; native: ")
              (nucleo-benchmark--search query candidates))
            (princ "\n")))))))

;;; language-search.el ends here
