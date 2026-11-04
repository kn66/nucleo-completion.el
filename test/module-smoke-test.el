;;; module-smoke-test.el --- Verify a built module loads -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;; Loaded after nucleo-completion.el by the prebuilt-module workflow.
;; Keep multilingual fixtures in a UTF-8 file: native Windows argument decoding
;; can replace non-ASCII --eval text before Emacs receives it.

;;; Code:

(unless (featurep 'nucleo-completion-module)
  (error "nucleo-completion module did not load"))

(unless (string= (nucleo-completion-module-version)
                 nucleo-completion-required-module-version)
  (error "loaded module version mismatch"))

(unless (equal (car (nucleo-completion-candidates
                     "fb" (list "fb" "foo-baz" "bar") nil nil nil 0 nil))
               (list "fb" "foo-baz"))
  (error "loaded module did not score candidates"))

(dolist (case '((nucleo-completion-migemo-regexp "kensaku" "検索")
                (nucleo-completion-pinyin-regexp "zhongguo" "中国")
                (nucleo-completion-hangul-regexp "ㄱㅅ" "감사")))
  (unless (string-match-p (funcall (car case) (cadr case)) (caddr case))
    (error "loaded module language search failed: %S" case)))

(unless (equal (nth 3 (nucleo-completion-candidates-with-regexp-groups
                       "nihon" (list "日本") nil nil nil nil 0 nil
                       (list (list "日本"))))
               (list "日本"))
  (error "loaded module combined language scan failed"))

;;; module-smoke-test.el ends here
