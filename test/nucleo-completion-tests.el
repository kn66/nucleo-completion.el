;;; nucleo-completion-tests.el --- Tests for nucleo-completion  -*- lexical-binding: t -*-

(require 'ert)
(require 'nucleo-completion)

(defun nucleo-completion-tests--plain (strings)
  (mapcar #'substring-no-properties strings))

(ert-deftest nucleo-completion-terms-test ()
  (should (equal (nucleo-completion--terms "  foo\tbar  baz\n")
                 '("foo" "bar" "baz")))
  (should (equal (nucleo-completion--terms "   ") nil)))

(ert-deftest nucleo-completion-subsequence-regexp-test ()
  (let ((case-fold-search nil)
        (regexp (concat "\\`" (nucleo-completion--subsequence-regexp "f.b"))))
    (should (string-match-p regexp "foo.bar"))
    (should-not (string-match-p regexp "foo-baz"))))

(ert-deftest nucleo-completion-platform-triples-test ()
  (let ((system-type 'gnu/linux)
        (system-configuration "x86_64-pc-linux-gnu"))
    (should (equal (nucleo-completion--platform-triples)
                   '("x86_64-unknown-linux-gnu"
                     "x86_64-unknown-linux-musl"))))
  (let ((system-type 'gnu/linux)
        (system-configuration "aarch64-unknown-linux-gnu"))
    (should (equal (nucleo-completion--platform-triples)
                   '("aarch64-unknown-linux-gnu"
                     "aarch64-unknown-linux-musl"))))
  (let ((system-type 'darwin)
        (system-configuration "aarch64-apple-darwin"))
    (should (equal (nucleo-completion--platform-triples)
                   '("aarch64-apple-darwin"))))
  (let ((system-type 'windows-nt)
        (system-configuration "x86_64-w64-mingw32"))
    (should (equal (nucleo-completion--platform-triples)
                   '("x86_64-pc-windows-gnu"
                     "x86_64-pc-windows-msvc")))))

(ert-deftest nucleo-completion-module-candidates-test ()
  (let* ((nucleo-completion--directory "/tmp/nucleo-completion/")
         (system-type 'gnu/linux)
         (system-configuration "x86_64-pc-linux-gnu")
         (candidates (nucleo-completion--module-candidates)))
    (should (member "/tmp/nucleo-completion/bin/x86_64-unknown-linux-gnu/libnucleo_completion_module.so"
                    candidates))
    (should (member "/tmp/nucleo-completion/bin/x86_64-unknown-linux-musl/libnucleo_completion_module.so"
                    candidates))
    (should (member "/tmp/nucleo-completion/target/release/libnucleo_completion_module.so"
                    candidates))
    (should (member "/tmp/nucleo-completion/target/debug/libnucleo_completion_module.so"
                    candidates))))

(ert-deftest nucleo-completion-filter-test ()
  (let ((completion-ignore-case nil))
    (should (equal (nucleo-completion-tests--plain
                    (nucleo-completion-all-completions
                     "fb" '("foobar" "fxxx" "foo-baz" "" "fb")))
                   '("fb" "foo-baz" "foobar")))))

(ert-deftest nucleo-completion-case-sensitivity-test ()
  (let ((candidates '("alpha" "Alpha" "ALPHA")))
    (let ((completion-ignore-case nil))
      (should (equal (nucleo-completion-tests--plain
                      (nucleo-completion-all-completions "A" candidates))
                     '("Alpha" "ALPHA"))))
    (let ((completion-ignore-case t))
      (should (equal (sort (nucleo-completion-tests--plain
                            (nucleo-completion-all-completions "A" candidates))
                           #'string<)
                     '("ALPHA" "Alpha" "alpha"))))))

(ert-deftest nucleo-completion-empty-input-test ()
  (let* ((completion-styles '(nucleo))
         (table '("b" "a" "c"))
         (md `(metadata (display-sort-function . ,(lambda (xs)
                                                    (sort xs #'string<)))))
         (all (completion-all-completions "" table nil 0 md))
         (sort-fn (completion-metadata-get md 'display-sort-function)))
    (should (equal (funcall sort-fn all) '("a" "b" "c")))))

(ert-deftest nucleo-completion-style-test ()
  (let* ((completion-styles '(nucleo))
         (table '("foobar" "fxxx" "foo-baz" "" "fb"))
         (md (completion-metadata "fb" table nil))
         (all (completion-all-completions "fb" table nil 2 md)))
    (setcdr (last all) nil)
    (should (equal (nucleo-completion-tests--plain all)
                   '("fb" "foo-baz" "foobar")))))

(ert-deftest nucleo-completion-space-separated-test ()
  (let* ((completion-styles '(nucleo))
         (table '("foo/bar" "bar/foo" "foobar" "foo-qux" "bar-baz"))
         (md (completion-metadata "foo bar" table nil))
         (all (completion-all-completions "foo bar" table nil 7 md)))
    (setcdr (last all) nil)
    (should (equal (nucleo-completion-tests--plain all)
                   '("foo/bar" "bar/foo" "foobar")))))

(ert-deftest nucleo-completion-regexp-function-test ()
  (let ((nucleo-completion-regexp-functions
         (list (lambda (term)
                 (pcase term
                   ("nihon" "日本")
                   ("go" "語")))))
        (completion-ignore-case nil))
    (should (equal (nucleo-completion-tests--plain
                    (nucleo-completion-all-completions
                     "nihon" '("日本語" "nihon-go" "英語")))
                   '("nihon-go" "日本語")))
    (should (equal (nucleo-completion-tests--plain
                    (nucleo-completion-all-completions
                     "nihon go" '("日本語" "nihon-go" "英語" "日本史")))
                   '("日本語" "nihon-go")))))

(ert-deftest nucleo-completion-regexp-function-list-and-invalid-test ()
  (let ((nucleo-completion-regexp-functions
         (list (lambda (term)
                 (pcase term
                   ("jp" '("日本" "["))
                   ("lang" '("語" nil 42)))))))
    (should (equal (nucleo-completion--regexp-function-regexps "jp")
                   '("日本")))
    (should (equal (nucleo-completion-tests--plain
                    (nucleo-completion-all-completions
                     "jp lang" '("日本語" "日本史" "英語" "jp-lang")))
                   '("日本語" "jp-lang")))))

(ert-deftest nucleo-completion-regexp-functions-buffer-local-disable-test ()
  (let ((nucleo-completion-regexp-functions
         (list (lambda (term)
                 (when (string= term "nihon")
                   "日本"))))
        (candidates '("日本語" "nihon-go" "英語")))
    (should (equal (nucleo-completion-tests--plain
                    (nucleo-completion-all-completions "nihon" candidates))
                   '("nihon-go" "日本語")))
    (with-temp-buffer
      (setq-local nucleo-completion-regexp-functions nil)
      (should (equal (nucleo-completion-tests--plain
                      (nucleo-completion-all-completions "nihon" candidates))
                     '("nihon-go"))))))

(ert-deftest nucleo-completion-sort-with-module-keeps-regexp-only-matches-test ()
  (let ((nucleo-completion-regexp-functions
         (list (lambda (term)
                 (when (string= term "nihon")
                   "日本")))))
    (cl-letf (((symbol-function 'nucleo-completion-scored-filter)
               (lambda (_needle candidates _ignore-case)
                 (cl-loop for candidate in candidates
                          when (string-match-p "roman" candidate)
                          collect (cons candidate 128)))))
      (should (equal (let* ((candidates '("日本語" "roman-nihon" "日本史"))
                            (regexp-scored
                             (nucleo-completion--regexp-filter-with-scores
                              "nihon" candidates)))
                       (nucleo-completion--sort-with-module
                        "nihon" candidates nil regexp-scored))
                     '("roman-nihon" "日本語" "日本史"))))))

(ert-deftest nucleo-completion-sort-with-module-scores-regexp-only-matches-test ()
  (let ((nucleo-completion-regexp-functions
         (list (lambda (term)
                 (when (string= term "nihon")
                   "日本")))))
    (cl-letf (((symbol-function 'nucleo-completion-scored-filter)
               (lambda (_needle _candidates _ignore-case)
                 '(("roman-nihon" . 128) ("nihon-tail" . 110)))))
      (let* ((candidates '("roman-nihon" "nihon-tail" "日本"))
             (regexp-scored
              (nucleo-completion--regexp-filter-with-scores
               "nihon" candidates)))
        (should (equal (nucleo-completion--sort-with-module
                        "nihon" candidates nil regexp-scored)
                       '("日本" "roman-nihon" "nihon-tail")))))))

(ert-deftest nucleo-completion-sort-ties-by-length-test ()
  (let ((nucleo-completion-sort-ties-by-length t)
        (nucleo-completion-sort-ties-alphabetically nil))
    (cl-letf (((symbol-function 'nucleo-completion-scored-filter)
               (lambda (_needle _candidates _ignore-case)
                 '(("alphabet" . 10) ("alpha" . 10) ("alpaca" . 11)))))
      (should (equal (nucleo-completion--module-filter
                      "alp" '("alphabet" "alpha" "alpaca") nil)
                     '("alpaca" "alpha" "alphabet"))))))

(ert-deftest nucleo-completion-sort-ties-alphabetically-test ()
  (let ((nucleo-completion-sort-ties-by-length nil)
        (nucleo-completion-sort-ties-alphabetically t))
    (cl-letf (((symbol-function 'nucleo-completion-scored-filter)
               (lambda (_needle _candidates _ignore-case)
                 '(("beta" . 10) ("alpha" . 10) ("aardvark" . 9)))))
      (should (equal (nucleo-completion--module-filter
                      "a" '("beta" "alpha" "aardvark") nil)
                     '("alpha" "beta" "aardvark"))))))

(ert-deftest nucleo-completion-sort-ties-length-before-alphabetical-test ()
  (let ((nucleo-completion-sort-ties-by-length t)
        (nucleo-completion-sort-ties-alphabetically t))
    (cl-letf (((symbol-function 'nucleo-completion-scored-filter)
               (lambda (_needle _candidates _ignore-case)
                 '(("bbb" . 10) ("aa" . 10) ("ccc" . 10) ("aaa" . 10)))))
      (should (equal (nucleo-completion--module-filter
                      "a" '("bbb" "aa" "ccc" "aaa") nil)
                     '("aa" "aaa" "bbb" "ccc"))))))

(ert-deftest nucleo-completion-adjust-metadata-test ()
  (let ((nucleo-completion--filtering-p t))
    (should (eq (completion-metadata-get
                 (nucleo-completion-adjust-metadata '(metadata (category . file)))
                 'display-sort-function)
                'identity)))
  (let ((nucleo-completion--filtering-p nil))
    (should (equal (nucleo-completion-adjust-metadata
                    '(metadata (category . file)))
                   '(metadata (category . file))))))

(ert-deftest nucleo-completion-highlight-test ()
  (should (equal-including-properties
           (nucleo-completion-highlight "fb" "foo-baz")
           #("foo-baz" 0 1 (face completions-common-part)
             4 5 (face completions-common-part)))))

(ert-deftest nucleo-completion-score-band-highlight-disabled-test ()
  (let ((nucleo-completion-highlight-score-bands nil))
    (let ((faces (ensure-list
                  (get-text-property
                   0 'face
                   (nucleo-completion--highlight-candidate
                    "foo" (copy-sequence "foo-bar") 100 100)))))
      (should-not (memq 'nucleo-completion-high-score-face faces))
      (should-not (memq 'nucleo-completion-low-score-face faces)))))

(ert-deftest nucleo-completion-score-band-highlight-test ()
  (let ((completion-ignore-case nil)
        (nucleo-completion-highlight-score-bands t)
        (nucleo-completion-high-score-ratio 0.85))
    (should (memq 'nucleo-completion-high-score-face
                  (ensure-list
                   (get-text-property
                    0 'face
                    (nucleo-completion--highlight-candidate
                     "foo" (copy-sequence "foo-bar") 10 100)))))
    (should (memq 'nucleo-completion-low-score-face
                  (ensure-list
                   (get-text-property
                    0 'face
                    (nucleo-completion--highlight-candidate
                     "fb" (copy-sequence "foo-bar") 10 100)))))))

(ert-deftest nucleo-completion-high-score-emphasis-test ()
  (let ((completion-ignore-case nil)
        (nucleo-completion-high-score-ratio 0.85))
    (let ((nucleo-completion-high-score-emphasis '(bold underline)))
      (should (equal (nucleo-completion--score-band-face "foo" "foo-bar" 10 100)
                     '(nucleo-completion-high-score-face bold underline))))
    (let ((nucleo-completion-high-score-emphasis '(bold)))
      (should (equal (nucleo-completion--score-band-face "foo" "foo-bar" 10 100)
                     '(nucleo-completion-high-score-face bold))))
    (let ((nucleo-completion-high-score-emphasis '(underline)))
      (should (equal (nucleo-completion--score-band-face "foo" "foo-bar" 10 100)
                     '(nucleo-completion-high-score-face underline))))
    (let ((nucleo-completion-high-score-emphasis nil))
      (should (equal (nucleo-completion--score-band-face "foo" "foo-bar" 10 100)
                     '(nucleo-completion-high-score-face))))))

(ert-deftest nucleo-completion-all-completions-score-band-test ()
  (let ((completion-ignore-case nil)
        (nucleo-completion-highlight-score-bands t)
        (nucleo-completion-high-score-ratio 0.85)
        (nucleo-completion-max-highlighted-completions 10))
    (cl-letf (((symbol-function 'nucleo-completion-filter)
               (lambda (_needle _candidates _ignore-case)
                 '("foo" "fob")))
              ((symbol-function 'nucleo-completion-scored-filter)
               (lambda (_needle _candidates _ignore-case)
                 '(("foo" . 100) ("fob" . 50)))))
      (let ((all (nucleo-completion-all-completions
                  "fo" '("foo" "fob" "bar"))))
        (should (equal (nucleo-completion-tests--plain all)
                       '("foo" "fob")))
        (should (memq 'nucleo-completion-high-score-face
                      (ensure-list (get-text-property 0 'face (car all)))))
        (should (memq 'nucleo-completion-low-score-face
                      (ensure-list (get-text-property 0 'face (cadr all)))))))))

(ert-deftest nucleo-completion-regexp-only-match-not-score-band-highlighted-test ()
  (let ((completion-ignore-case nil)
        (nucleo-completion-highlight-score-bands t)
        (nucleo-completion-high-score-ratio 0.85)
        (nucleo-completion-max-highlighted-completions 10)
        (nucleo-completion-regexp-functions
         (list (lambda (term)
                 (when (string= term "nihon")
                   "日本")))))
    (cl-letf (((symbol-function 'nucleo-completion-filter)
               (lambda (_needle _candidates _ignore-case)
                 '("roman-nihon")))
              ((symbol-function 'nucleo-completion-scored-filter)
               (lambda (_needle candidates _ignore-case)
                 (cl-loop for candidate in candidates
                          when (string= candidate "roman-nihon")
                          collect (cons candidate 128)))))
      (let* ((all (nucleo-completion-all-completions
                   "nihon" '("日本語" "roman-nihon")))
             (plain (nucleo-completion-tests--plain all))
             (regexp-only (nth (cl-position "日本語" plain :test #'equal) all))
             (module-match (nth (cl-position "roman-nihon" plain :test #'equal)
                                all))
             (regexp-only-faces (ensure-list
                                 (get-text-property 0 'face regexp-only)))
             (module-faces (ensure-list
                            (get-text-property 0 'face module-match))))
        (should (equal plain '("roman-nihon" "日本語")))
        (should (memq 'completions-common-part regexp-only-faces))
        (should-not (memq 'nucleo-completion-high-score-face regexp-only-faces))
        (should-not (memq 'nucleo-completion-low-score-face regexp-only-faces))
        (should (memq 'nucleo-completion-high-score-face module-faces))))))

(ert-deftest nucleo-completion-space-separated-highlight-test ()
  (should (equal-including-properties
           (nucleo-completion-highlight "foo bar" "foo/bar")
           #("foo/bar" 0 3 (face completions-common-part)
             4 7 (face completions-common-part)))))

(ert-deftest nucleo-completion-regexp-function-highlight-test ()
  (let ((nucleo-completion-regexp-functions
         (list (lambda (term)
                 (pcase term
                   ("nihon" "日本")
                   ("go" "語"))))))
    (should (equal-including-properties
             (nucleo-completion-highlight "nihon" "日本語")
             #("日本語" 0 2 (face completions-common-part))))
    (should (equal-including-properties
             (nucleo-completion-highlight "nihon go" "日本語")
             #("日本語" 0 3 (face completions-common-part))))))

;;; nucleo-completion-tests.el ends here
