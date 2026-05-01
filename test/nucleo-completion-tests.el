;;; nucleo-completion-tests.el --- Tests for nucleo-completion  -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'nucleo-completion)

(defun nucleo-completion-tests--plain (strings)
  (mapcar #'substring-no-properties strings))

(defun nucleo-completion-tests--bundle (triples &optional return-all-scores)
  "Build a module-result bundle from TRIPLES.
Each element of TRIPLES has the form (CAND SCORE INDICES) and is
placed verbatim into the top-info slot.  CANDIDATES is the
candidate list (in the same order as TRIPLES).  FULL-SCORES is
populated when RETURN-ALL-SCORES is non-nil."
  (list (mapcar #'car triples)
        triples
        (when return-all-scores
          (mapcar #'cadr triples))))

(defmacro nucleo-completion-tests--with-mock-candidates (triples &rest body)
  "Stub `nucleo-completion-candidates' to return TRIPLES wrapped as a bundle."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'nucleo-completion-candidates)
              (lambda (_needle _candidates _ignore-case _by-length
                               _alphabetically _limit
                               &optional return-all-scores)
                (nucleo-completion-tests--bundle ,triples
                                                 return-all-scores))))
     ,@body))

(defun nucleo-completion-tests--high-score-face-p (faces)
  (cl-some (lambda (face)
             (eq face 'nucleo-completion-high-score-face))
           (ensure-list faces)))

(defvar nucleo-completion-tests--regexp-calls 0)

(defun nucleo-completion-tests--nihon-regexp (term)
  (setq nucleo-completion-tests--regexp-calls
        (1+ nucleo-completion-tests--regexp-calls))
  (when (string= term "nihon")
    "日本"))

(ert-deftest nucleo-completion-terms-test ()
  (should (equal (nucleo-completion--terms "  foo\tbar  baz\n")
                 '("foo" "bar" "baz")))
  (should (equal (nucleo-completion--terms "   ") nil)))

(ert-deftest nucleo-completion-terms-cache-test ()
  (let ((nucleo-completion--terms-cache (make-hash-table :test #'equal)))
    (should (eq (nucleo-completion--terms "foo bar")
                (nucleo-completion--terms "foo bar")))))

(ert-deftest nucleo-completion-subsequence-regexp-test ()
  (let ((case-fold-search nil)
        (regexp (concat "\\`" (nucleo-completion--subsequence-regexp "f.b"))))
    (should (string-match-p regexp "foo.bar"))
    (should-not (string-match-p regexp "foo-baz"))))

(ert-deftest nucleo-completion-subsequence-regexp-cache-test ()
  (let ((nucleo-completion--subsequence-regexp-cache
         (make-hash-table :test #'equal)))
    (should (eq (nucleo-completion--subsequence-regexp "fb")
                (nucleo-completion--subsequence-regexp "fb")))))

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

(ert-deftest nucleo-completion-no-dynamic-module-support-test ()
  (let ((nucleo-completion-module-load-errors 'stale))
    (cl-letf (((symbol-function 'nucleo-completion--dynamic-modules-supported-p)
               (lambda () nil))
              ((symbol-function 'module-load)
               (lambda (_file)
                 (error "module-load must not be called"))))
      (should-not (nucleo-completion--module-candidates))
      (nucleo-completion--load-module)
      (should (equal nucleo-completion-module-load-errors 'stale)))))

(ert-deftest nucleo-completion-module-load-errors-test ()
  (let ((nucleo-completion-module-load-errors 'stale)
        (nucleo-completion-report-module-load-errors nil)
        (original-featurep (symbol-function 'featurep)))
    (cl-letf (((symbol-function 'featurep)
               (lambda (feature &optional subfeature)
                 (if (eq feature 'nucleo-completion-module)
                     nil
                   (funcall original-featurep feature subfeature))))
              ((symbol-function 'nucleo-completion--module-candidates)
               (lambda () '("/tmp/nucleo-a.so" "/tmp/nucleo-b.so")))
              ((symbol-function 'file-readable-p)
               (lambda (_file) t))
              ((symbol-function 'module-load)
               (lambda (file)
                 (error "cannot load %s" file))))
      (nucleo-completion--load-module)
      (should (equal nucleo-completion-module-load-errors
                     '(("/tmp/nucleo-a.so" . "cannot load /tmp/nucleo-a.so")
                       ("/tmp/nucleo-b.so" . "cannot load /tmp/nucleo-b.so")))))))

(ert-deftest nucleo-completion-custom-group-test ()
  (let ((members (get 'nucleo-completion 'custom-group)))
    (dolist (symbol '(nucleo-completion-max-highlighted-completions
                      nucleo-completion-regexp-functions
                      nucleo-completion-persistent-regexp-cache-size
                      nucleo-completion-long-candidate-threshold
                      nucleo-completion-long-candidate-regexp-threshold
                      nucleo-completion-long-candidate-highlight-threshold
                      nucleo-completion-scrub-non-unicode-candidates
                      nucleo-completion-sort-ties-by-length
                      nucleo-completion-sort-ties-alphabetically
                      nucleo-completion-highlight-score-bands
                      nucleo-completion-high-score-ratio
                      nucleo-completion-high-score-emphasis
                      nucleo-completion-report-module-load-errors))
      (should (member (list symbol 'custom-variable) members)))))

(ert-deftest nucleo-completion-highlight-limit-sanitizes-test ()
  (let ((nucleo-completion-max-highlighted-completions 3))
    (should (= (nucleo-completion--highlight-limit) 3)))
  (let ((nucleo-completion-max-highlighted-completions -1))
    (should (= (nucleo-completion--highlight-limit) 0)))
  (let ((nucleo-completion-max-highlighted-completions 'invalid))
    (should (= (nucleo-completion--highlight-limit) 0))))

(ert-deftest nucleo-completion-high-score-ratio-sanitizes-test ()
  (let ((nucleo-completion-high-score-ratio 0.5))
    (should (= (nucleo-completion--high-score-ratio) 0.5)))
  (let ((nucleo-completion-high-score-ratio -1))
    (should (= (nucleo-completion--high-score-ratio) 0.0)))
  (let ((nucleo-completion-high-score-ratio 2))
    (should (= (nucleo-completion--high-score-ratio) 1.0)))
  (let ((nucleo-completion-high-score-ratio 'invalid))
    (should (= (nucleo-completion--high-score-ratio) 0.85))))

(ert-deftest nucleo-completion-exact-word-regexps-cache-test ()
  (let ((nucleo-completion--exact-word-regexp-cache
         (make-hash-table :test #'equal)))
    (should (eq (nucleo-completion--exact-word-regexps "foo bar")
                (nucleo-completion--exact-word-regexps "foo bar")))))

(ert-deftest nucleo-completion-long-candidate-threshold-sanitizes-test ()
  (let ((nucleo-completion-long-candidate-threshold 3))
    (should (= (nucleo-completion--long-candidate-threshold) 3)))
  (let ((nucleo-completion-long-candidate-threshold nil))
    (should-not (nucleo-completion--long-candidate-threshold)))
  (let ((nucleo-completion-long-candidate-threshold -1))
    (should-not (nucleo-completion--long-candidate-threshold)))
  (let ((nucleo-completion-long-candidate-threshold 'invalid))
    (should-not (nucleo-completion--long-candidate-threshold))))

(ert-deftest nucleo-completion-long-candidate-regexp-threshold-sanitizes-test ()
  (let ((nucleo-completion-long-candidate-regexp-threshold 3))
    (should (= (nucleo-completion--long-candidate-regexp-threshold) 3)))
  (let ((nucleo-completion-long-candidate-regexp-threshold nil))
    (should-not (nucleo-completion--long-candidate-regexp-threshold)))
  (let ((nucleo-completion-long-candidate-regexp-threshold -1))
    (should-not (nucleo-completion--long-candidate-regexp-threshold)))
  (let ((nucleo-completion-long-candidate-regexp-threshold 'invalid))
    (should-not (nucleo-completion--long-candidate-regexp-threshold))))

(ert-deftest nucleo-completion-long-candidate-highlight-threshold-sanitizes-test ()
  (let ((nucleo-completion-long-candidate-highlight-threshold 3))
    (should (= (nucleo-completion--long-candidate-highlight-threshold) 3)))
  (let ((nucleo-completion-long-candidate-highlight-threshold nil))
    (should-not (nucleo-completion--long-candidate-highlight-threshold)))
  (let ((nucleo-completion-long-candidate-highlight-threshold -1))
    (should-not (nucleo-completion--long-candidate-highlight-threshold)))
  (let ((nucleo-completion-long-candidate-highlight-threshold 'invalid))
    (should-not (nucleo-completion--long-candidate-highlight-threshold))))

(ert-deftest nucleo-completion-long-candidate-regexp-threshold-inherit-test ()
  "When set to `inherit', the regexp threshold follows the main one."
  (let ((nucleo-completion-long-candidate-threshold 7)
        (nucleo-completion-long-candidate-regexp-threshold 'inherit))
    (should (= (nucleo-completion--long-candidate-regexp-threshold) 7)))
  (let ((nucleo-completion-long-candidate-threshold nil)
        (nucleo-completion-long-candidate-regexp-threshold 'inherit))
    (should-not (nucleo-completion--long-candidate-regexp-threshold)))
  (let ((nucleo-completion-long-candidate-threshold 9)
        (nucleo-completion-long-candidate-regexp-threshold 4))
    (should (= (nucleo-completion--long-candidate-regexp-threshold) 4))))

(ert-deftest nucleo-completion-long-candidate-highlight-threshold-inherit-test ()
  "When set to `inherit', the highlight threshold follows the main one."
  (let ((nucleo-completion-long-candidate-threshold 7)
        (nucleo-completion-long-candidate-highlight-threshold 'inherit))
    (should (= (nucleo-completion--long-candidate-highlight-threshold) 7)))
  (let ((nucleo-completion-long-candidate-threshold nil)
        (nucleo-completion-long-candidate-highlight-threshold 'inherit))
    (should-not (nucleo-completion--long-candidate-highlight-threshold)))
  (let ((nucleo-completion-long-candidate-threshold 9)
        (nucleo-completion-long-candidate-highlight-threshold 4))
    (should (= (nucleo-completion--long-candidate-highlight-threshold) 4))))

(ert-deftest nucleo-completion-unicode-string-p-test ()
  (should (nucleo-completion--unicode-string-p ""))
  (should (nucleo-completion--unicode-string-p "abc"))
  (should (nucleo-completion--unicode-string-p "ABC"))
  (let ((tofu (string #x200000)))
    (should-not (nucleo-completion--unicode-string-p tofu))
    (should-not (nucleo-completion--unicode-string-p
                 (concat "abc" tofu)))))

(ert-deftest nucleo-completion-scrub-non-unicode-string-test ()
  (let ((clean "abc")
        (tofu (string #x200001)))
    (should (eq (nucleo-completion--scrub-non-unicode-string clean) clean))
    (should (equal (nucleo-completion--scrub-non-unicode-string
                    (concat "abc" tofu))
                   "abc"))
    (should (equal (nucleo-completion--scrub-non-unicode-string
                    (concat "a" tofu "b" tofu "c"))
                   "abc"))))

(ert-deftest nucleo-completion-scrub-candidates-keeps-list-when-clean-test ()
  (let* ((candidates '("foo" "bar"))
         (result (nucleo-completion--scrub-candidates candidates)))
    (should (eq (car result) candidates))
    (should-not (cdr result))))

(ert-deftest nucleo-completion-scrub-candidates-strips-non-unicode-test ()
  (let* ((tofu (string #x200002))
         (consult-cand (concat "abc-def" tofu))
         (result (nucleo-completion--scrub-candidates
                  (list "foo" consult-cand)))
         (cleaned (car result))
         (map (cdr result)))
    (should (equal cleaned '("foo" "abc-def")))
    (should map)
    (should (equal (gethash "abc-def" map) (list consult-cand)))))

(ert-deftest nucleo-completion-module-results-restores-original-candidates-test ()
  "Tofu-bearing candidates round-trip through the module unchanged."
  (let* ((tofu (string #x200003))
         (consult-cand (concat "abc-def" tofu))
         (nucleo-completion-scrub-non-unicode-candidates t)
         (received-input nil))
    (cl-letf (((symbol-function 'nucleo-completion-candidates)
               (lambda (_needle candidates _ignore-case _by-length
                                _alphabetically _limit
                                &optional return-all-scores)
                 (setq received-input candidates)
                 (nucleo-completion-tests--bundle
                  (mapcar (lambda (c) (list c 100 nil)) candidates)
                  return-all-scores))))
      (let* ((bundle (nucleo-completion--module-results
                      "abc" (list "foo" consult-cand) nil 0 t))
             (returned (nucleo-completion--bundle-candidates bundle))
             (top-info (nucleo-completion--bundle-top-info bundle)))
        (should (equal received-input '("foo" "abc-def")))
        (should (eq (cadr returned) consult-cand))
        (should (eq (car (cadr top-info)) consult-cand))))))

(ert-deftest nucleo-completion-module-results-restores-duplicate-scrubbed-candidates-test ()
  "Candidates with the same scrubbed text restore to distinct originals."
  (let* ((tofu-a (string #x200004))
         (tofu-b (string #x200005))
         (cand-a (concat "same" tofu-a))
         (cand-b (concat "same" tofu-b))
         (nucleo-completion-scrub-non-unicode-candidates t)
         received-input)
    (cl-letf (((symbol-function 'nucleo-completion-candidates)
               (lambda (_needle candidates _ignore-case _by-length
                                _alphabetically _limit
                                &optional return-all-scores)
                 (setq received-input candidates)
                 (nucleo-completion-tests--bundle
                  (mapcar (lambda (c) (list c 100 nil)) candidates)
                  return-all-scores))))
      (let* ((bundle (nucleo-completion--module-results
                      "same" (list cand-a cand-b) nil 2 t))
             (returned (nucleo-completion--bundle-candidates bundle))
             (top-info (nucleo-completion--bundle-top-info bundle)))
        (should (equal received-input '("same" "same")))
        (should (eq (car returned) cand-a))
        (should (eq (cadr returned) cand-b))
        (should (eq (caar top-info) cand-a))
        (should (eq (caadr top-info) cand-b))))))

(ert-deftest nucleo-completion-module-results-skips-scrub-by-default-test ()
  "Ordinary module calls avoid the per-candidate scrub scan by default."
  (let ((nucleo-completion-scrub-non-unicode-candidates nil))
    (cl-letf (((symbol-function 'nucleo-completion--scrub-candidates)
               (lambda (_candidates)
                 (error "scrub should not run")))
              ((symbol-function 'nucleo-completion-candidates)
               (lambda (_needle candidates _ignore-case _by-length
                                _alphabetically _limit
                                &optional return-all-scores)
                 (should (equal candidates '("foo" "bar")))
                 (nucleo-completion-tests--bundle
                  '(("foo" 100 nil) ("bar" 90 nil))
                  return-all-scores))))
      (should (equal (nucleo-completion--bundle-candidates
                      (nucleo-completion--module-results
                       "f" '("foo" "bar") nil 0))
                     '("foo" "bar"))))))

(ert-deftest nucleo-completion-split-scored-candidates-test ()
  (let ((nucleo-completion-long-candidate-threshold 3))
    (should (equal (nucleo-completion--split-scored-candidates
                    '("a" "abcd" "bc" "cdefg"))
                   '(("a" "bc") ("abcd" "cdefg"))))))

(ert-deftest nucleo-completion-filter-test ()
  (let ((completion-ignore-case nil))
    (should (equal (nucleo-completion-tests--plain
                    (nucleo-completion-all-completions
                     "fb" '("foobar" "fxxx" "foo-baz" "" "fb")))
                   '("fb" "foo-baz" "foobar")))))

(ert-deftest nucleo-completion-fallback-filter-only-test ()
  (let ((completion-ignore-case nil)
        (nucleo-completion-sort-ties-by-length t)
        (nucleo-completion-sort-ties-alphabetically t)
        (nucleo-completion-highlight-score-bands t)
        (nucleo-completion-max-highlighted-completions 10))
    (cl-letf (((symbol-function 'nucleo-completion--module-ready-p)
               (lambda () nil))
              ((symbol-function 'nucleo-completion-candidates)
               (lambda (&rest _)
                 (error "fallback must not call the Rust candidate API"))))
      (let ((all (nucleo-completion-all-completions
                  "fb" '("foo-baz" "fb" "foobar" "bar"))))
        (should (equal (nucleo-completion-tests--plain all)
                       '("foo-baz" "fb" "foobar")))
        (dolist (candidate all)
          (let ((faces (ensure-list (get-text-property 0 'face candidate))))
            (should-not (memq 'nucleo-completion-high-score-face faces))
            (should-not (memq 'nucleo-completion-low-score-face faces))))))))

(ert-deftest nucleo-completion-native-module-smoke-test ()
  (unless (nucleo-completion--module-ready-p)
    (ert-skip "Rust module is not available"))
  (let ((completion-ignore-case nil))
    (should (equal (nucleo-completion--module-filter
                    "fb" '("foobar" "fxxx" "foo-baz" "" "fb") nil)
                   '("fb" "foo-baz" "foobar")))))

(ert-deftest nucleo-completion-module-path-sanitizes-highlight-limit-test ()
  (let ((completion-ignore-case nil)
        (nucleo-completion-max-highlighted-completions -10))
    (cl-letf (((symbol-function 'nucleo-completion--module-ready-p)
               (lambda () t))
              ((symbol-function 'nucleo-completion-candidates)
               (lambda (_needle _candidates _ignore-case _by-length
                                _alphabetically limit
                                &optional return-all-scores)
                 (should (= limit 0))
                 (nucleo-completion-tests--bundle '(("fb" 100 nil))
                                                  return-all-scores))))
      (should (equal (nucleo-completion-tests--plain
                      (nucleo-completion-all-completions
                       "fb" '("fb" "foo-baz")))
                     '("fb"))))))

(ert-deftest nucleo-completion-long-candidates-skip-module-scoring-test ()
  (let ((completion-ignore-case nil)
        (nucleo-completion-long-candidate-threshold 5)
        (nucleo-completion-highlight-score-bands t)
        (nucleo-completion-max-highlighted-completions 10)
        (long-match "foo-baz")
        (long-miss "foo-xxx"))
    (cl-letf (((symbol-function 'nucleo-completion--module-ready-p)
               (lambda () t))
              ((symbol-function 'nucleo-completion-candidates)
               (lambda (_needle candidates _ignore-case _by-length
                                _alphabetically _limit
                                &optional return-all-scores)
                 (should (equal candidates '("fb")))
                 (nucleo-completion-tests--bundle '(("fb" 100 (0 1)))
                                                  return-all-scores))))
      (let ((all (nucleo-completion-all-completions
                  "fb" (list long-match "fb" long-miss))))
        (should (equal (nucleo-completion-tests--plain all)
                       '("fb" "foo-baz")))
        (should-not
         (memq 'nucleo-completion-high-score-face
               (ensure-list (get-text-property 0 'face (cadr all)))))))))

(ert-deftest nucleo-completion-long-candidates-skip-regexp-expanders-test ()
  (let ((completion-ignore-case nil)
        (nucleo-completion-long-candidate-regexp-threshold 2)
        (nucleo-completion-regexp-functions
         (list (lambda (term)
                 (when (string= term "jp")
                   "日本")))))
    (cl-letf (((symbol-function 'nucleo-completion--module-ready-p)
               (lambda () nil)))
      (should (equal (nucleo-completion-tests--plain
                      (nucleo-completion-all-completions
                       "jp" '("日本" "日本語" "jp-long")))
                     '("日本" "jp-long"))))))

(ert-deftest nucleo-completion-long-candidates-skip-regexp-highlight-test ()
  (let ((completion-ignore-case nil)
        (nucleo-completion-long-candidate-regexp-threshold 2)
        (nucleo-completion-regexp-functions
         (list (lambda (term)
                 (when (string= term "jp")
                   "日本")))))
    (should-not
     (get-text-property
      0 'face
      (nucleo-completion-highlight "jp" (copy-sequence "日本語"))))))

(ert-deftest nucleo-completion-long-candidates-skip-match-highlight-test ()
  (let ((nucleo-completion-long-candidate-highlight-threshold 3))
    (should-not
     (get-text-property
      0 'face
      (nucleo-completion-highlight "fb" (copy-sequence "foo-baz"))))))

(ert-deftest nucleo-completion-fuzzy-only-regexp-groups-are-lazy-test ()
  (let ((completion-ignore-case nil)
        (nucleo-completion-long-candidate-regexp-threshold 100)
        (calls 0)
        (original (symbol-function 'nucleo-completion--term-regexp-groups)))
    (cl-letf (((symbol-function 'nucleo-completion--term-regexp-groups)
               (lambda (needle &optional fuzzy-only)
                 (when fuzzy-only
                   (setq calls (1+ calls)))
                 (funcall original needle fuzzy-only))))
      (nucleo-completion--regexp-filter-pairs "fb" '("fb" "foo-baz"))
      (should (= calls 0)))))

(ert-deftest nucleo-completion-interrupt-keeps-last-filtered-result-test ()
  (let ((completion-ignore-case nil)
        (nucleo-completion--current-prefix "")
        (nucleo-completion--current-result '("fb")))
    (cl-letf (((symbol-function 'nucleo-completion-candidates)
               (lambda (&rest _)
                 (throw 'nucleo-completion-interrupted t))))
      (should
       (catch 'nucleo-completion-interrupted
         (nucleo-completion--all-completions-1
          "fb" '("fb" "bar" "foo-baz" "unmatched") nil nil)
         nil))
      (should (equal nucleo-completion--current-result '("fb"))))))

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
                   '("日本語" "nihon-go")))
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

(ert-deftest nucleo-completion-regexp-functions-cached-per-completion-test ()
  (let ((nucleo-completion-tests--regexp-calls 0)
        (nucleo-completion-max-highlighted-completions 10)
        (nucleo-completion-regexp-functions
         (list #'nucleo-completion-tests--nihon-regexp)))
    (cl-letf (((symbol-function 'nucleo-completion--module-ready-p)
               (lambda () nil)))
      (should (equal (nucleo-completion-tests--plain
                      (nucleo-completion-all-completions
                       "nihon" '("日本語" "nihon-go" "英語")))
                     '("日本語" "nihon-go")))
      (should (= nucleo-completion-tests--regexp-calls 1)))))

(ert-deftest nucleo-completion-persistent-regexp-cache-disabled-by-default-test ()
  (let ((nucleo-completion-tests--regexp-calls 0)
        (nucleo-completion-persistent-regexp-cache-size nil)
        (nucleo-completion-regexp-functions
         (list #'nucleo-completion-tests--nihon-regexp)))
    (nucleo-completion-clear-persistent-regexp-cache)
    (cl-letf (((symbol-function 'nucleo-completion--module-ready-p)
               (lambda () nil)))
      (nucleo-completion-all-completions
       "nihon" '("日本語" "nihon-go" "英語"))
      (nucleo-completion-all-completions
       "nihon" '("日本語" "nihon-go" "英語"))
      (should (= nucleo-completion-tests--regexp-calls 2)))))

(ert-deftest nucleo-completion-persistent-regexp-cache-test ()
  (let ((nucleo-completion-tests--regexp-calls 0)
        (nucleo-completion-persistent-regexp-cache-size 1024)
        (nucleo-completion-regexp-functions
         (list #'nucleo-completion-tests--nihon-regexp)))
    (nucleo-completion-clear-persistent-regexp-cache)
    (cl-letf (((symbol-function 'nucleo-completion--module-ready-p)
               (lambda () nil)))
      (should (equal (nucleo-completion-tests--plain
                      (nucleo-completion-all-completions
                       "nihon" '("日本語" "nihon-go" "英語")))
                     '("日本語" "nihon-go")))
      (should (equal (nucleo-completion-tests--plain
                      (nucleo-completion-all-completions
                       "nihon" '("日本語" "nihon-go" "英語")))
                     '("日本語" "nihon-go")))
      (should (= nucleo-completion-tests--regexp-calls 1)))))

(ert-deftest nucleo-completion-persistent-regexp-cache-keys-function-list-test ()
  (let ((nucleo-completion-persistent-regexp-cache-size 1024)
        (calls-a 0)
        (calls-b 0))
    (nucleo-completion-clear-persistent-regexp-cache)
    (let ((nucleo-completion-regexp-functions
           (list (lambda (_term)
                   (setq calls-a (1+ calls-a))
                   "日本"))))
      (should (equal (nucleo-completion--regexp-function-regexps "nihon")
                     '("日本"))))
    (let ((nucleo-completion-regexp-functions
           (list (lambda (_term)
                   (setq calls-b (1+ calls-b))
                   "語"))))
      (should (equal (nucleo-completion--regexp-function-regexps "nihon")
                     '("語"))))
    (should (= calls-a 1))
    (should (= calls-b 1))))

(ert-deftest nucleo-completion-regexp-functions-buffer-local-disable-test ()
  (let ((nucleo-completion-regexp-functions
         (list (lambda (term)
                 (when (string= term "nihon")
                   "日本"))))
        (candidates '("日本語" "nihon-go" "英語")))
    (should (equal (nucleo-completion-tests--plain
                    (nucleo-completion-all-completions "nihon" candidates))
                   '("日本語" "nihon-go")))
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
    (cl-letf (((symbol-function 'nucleo-completion-candidates)
               (lambda (_needle candidates _ignore-case _by-length
                                _alphabetically _limit
                                &optional return-all-scores)
                 (nucleo-completion-tests--bundle
                  (cl-loop for candidate in candidates
                           when (string-match-p "roman" candidate)
                           collect (list candidate 128 nil))
                  return-all-scores))))
      (should (equal (let* ((candidates '("日本語" "roman-nihon" "日本史"))
                            (regexp-pairs
                             (nucleo-completion--regexp-filter-pairs
                              "nihon" candidates))
                            (bundle
                             (nucleo-completion--module-results
                              "nihon" (mapcar #'car regexp-pairs) nil 0))
                            (module-results
                             (nucleo-completion--bundle-top-info bundle)))
                       (mapcar #'nucleo-completion--result-candidate
                               (nucleo-completion--merge-regexp-results
                                regexp-pairs module-results)))
                     '("日本語" "日本史" "roman-nihon"))))))

(ert-deftest nucleo-completion-module-skips-regexp-filter-for-module-matches-test ()
  (let ((nucleo-completion-regexp-functions
         (list (lambda (term)
                 (when (string= term "nihon")
                   "日本"))))
        seen)
    (cl-letf (((symbol-function 'nucleo-completion-candidates)
               (lambda (_needle candidates _ignore-case _by-length
                                _alphabetically _limit
                                &optional return-all-scores)
                 (nucleo-completion-tests--bundle
                  (cl-loop for candidate in candidates
                           when (string-match-p "roman" candidate)
                           collect (list candidate 128 nil))
                  return-all-scores)))
              ((symbol-function 'nucleo-completion--regexp-match-p)
               (lambda (regexp-groups candidate)
                 (push candidate seen)
                 (cl-every (lambda (regexps)
                             (cl-some (lambda (regexp)
                                        (string-match-p regexp candidate))
                                      regexps))
                           regexp-groups))))
      (should (equal (nucleo-completion-tests--plain
                      (nucleo-completion-all-completions
                       "nihon" '("roman-nihon" "日本語" "miss")))
                     '("日本語" "roman-nihon")))
      (should (equal (nreverse seen) '("日本語" "miss"))))))

(ert-deftest nucleo-completion-sort-with-module-scores-regexp-only-matches-test ()
  (let ((nucleo-completion-regexp-functions
         (list (lambda (term)
                 (when (string= term "nihon")
                   "日本")))))
    (cl-letf (((symbol-function 'nucleo-completion-candidates)
               (lambda (_needle _candidates _ignore-case _by-length
                                _alphabetically _limit
                                &optional return-all-scores)
                 (nucleo-completion-tests--bundle
                  '(("roman-nihon" 128 nil) ("nihon-tail" 110 nil))
                  return-all-scores))))
      (let* ((candidates '("roman-nihon" "nihon-tail" "日本"))
             (regexp-pairs
              (nucleo-completion--regexp-filter-pairs
               "nihon" candidates))
             (bundle
              (nucleo-completion--module-results
               "nihon" (mapcar #'car regexp-pairs) nil 0))
             (module-results
              (nucleo-completion--bundle-top-info bundle)))
        (should (equal (mapcar #'nucleo-completion--result-candidate
                               (nucleo-completion--merge-regexp-results
                                regexp-pairs module-results))
                       '("日本" "roman-nihon" "nihon-tail")))))))

(ert-deftest nucleo-completion-sort-ties-by-length-test ()
  (let ((nucleo-completion-sort-ties-by-length t)
        (nucleo-completion-sort-ties-alphabetically nil))
    (cl-letf (((symbol-function 'nucleo-completion-candidates)
               (lambda (needle candidates ignore-case by-length alphabetically
                               limit &optional return-all-scores)
                 (should (equal needle "alp"))
                 (should (equal candidates '("alphabet" "alpha" "alpaca")))
                 (should-not ignore-case)
                 (should by-length)
                 (should-not alphabetically)
                 (should (= limit 0))
                 (nucleo-completion-tests--bundle
                  '(("alpaca" 11 nil) ("alpha" 10 nil) ("alphabet" 10 nil))
                  return-all-scores))))
      (should (equal (nucleo-completion--module-filter
                      "alp" '("alphabet" "alpha" "alpaca") nil)
                     '("alpaca" "alpha" "alphabet"))))))

(ert-deftest nucleo-completion-sort-ties-alphabetically-test ()
  (let ((nucleo-completion-sort-ties-by-length nil)
        (nucleo-completion-sort-ties-alphabetically t))
    (cl-letf (((symbol-function 'nucleo-completion-candidates)
               (lambda (needle candidates ignore-case by-length alphabetically
                               limit &optional return-all-scores)
                 (should (equal needle "a"))
                 (should (equal candidates '("beta" "alpha" "aardvark")))
                 (should-not ignore-case)
                 (should-not by-length)
                 (should alphabetically)
                 (should (= limit 0))
                 (nucleo-completion-tests--bundle
                  '(("alpha" 10 nil) ("beta" 10 nil) ("aardvark" 9 nil))
                  return-all-scores))))
      (should (equal (nucleo-completion--module-filter
                      "a" '("beta" "alpha" "aardvark") nil)
                     '("alpha" "beta" "aardvark"))))))

(ert-deftest nucleo-completion-sort-ties-length-before-alphabetical-test ()
  (let ((nucleo-completion-sort-ties-by-length t)
        (nucleo-completion-sort-ties-alphabetically t))
    (cl-letf (((symbol-function 'nucleo-completion-candidates)
               (lambda (needle candidates ignore-case by-length alphabetically
                               limit &optional return-all-scores)
                 (should (equal needle "a"))
                 (should (equal candidates '("bbb" "aa" "ccc" "aaa")))
                 (should-not ignore-case)
                 (should by-length)
                 (should alphabetically)
                 (should (= limit 0))
                 (nucleo-completion-tests--bundle
                  '(("aa" 10 nil) ("aaa" 10 nil)
                    ("bbb" 10 nil) ("ccc" 10 nil))
                  return-all-scores))))
      (should (equal (nucleo-completion--module-filter
                      "a" '("bbb" "aa" "ccc" "aaa") nil)
                     '("aa" "aaa" "bbb" "ccc"))))))

(ert-deftest nucleo-completion-sort-ties-with-scores-uses-module-test ()
  (let ((nucleo-completion-sort-ties-by-length t)
        (nucleo-completion-sort-ties-alphabetically t))
    (cl-letf (((symbol-function 'nucleo-completion-candidates)
               (lambda (needle candidates ignore-case by-length alphabetically
                               limit &optional return-all-scores)
                 (should (equal needle "a"))
                 (should (equal candidates '("bbb" "aa" "ccc" "aaa")))
                 (should-not ignore-case)
                 (should by-length)
                 (should alphabetically)
                 (should (= limit 0))
                 (should return-all-scores)
                 (nucleo-completion-tests--bundle
                  '(("aa" 10 nil) ("aaa" 10 nil)
                    ("bbb" 10 nil) ("ccc" 10 nil))
                  return-all-scores))))
      (should (equal (nucleo-completion--module-filter-with-scores
                      "a" '("bbb" "aa" "ccc" "aaa") nil)
                     '(("aa" . 10) ("aaa" . 10)
                       ("bbb" . 10) ("ccc" . 10)))))))

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

(ert-deftest nucleo-completion-precomputed-highlight-test ()
  (should (equal-including-properties
           (nucleo-completion--highlight-candidate
            "fb" (copy-sequence "foo-baz") nil nil '(4 5))
           #("foo-baz" 4 6 (face completions-common-part)))))

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
    (should (nucleo-completion-tests--high-score-face-p
             (get-text-property
              0 'face
              (nucleo-completion--highlight-candidate
               "foo" (copy-sequence "foo-bar") 10 100))))
    (should (memq 'nucleo-completion-low-score-face
                  (ensure-list
                   (get-text-property
                    0 'face
                    (nucleo-completion--highlight-candidate
                     "fb" (copy-sequence "foo-bar") 10 100)))))))

(ert-deftest nucleo-completion-high-score-emphasis-precedes-match-face-test ()
  (let ((completion-ignore-case nil)
        (nucleo-completion-highlight-score-bands t)
        (nucleo-completion-high-score-ratio 0.85)
        (nucleo-completion-high-score-emphasis '(bold underline)))
    (should (equal
             (get-text-property
              0 'face
              (nucleo-completion--highlight-candidate
               "foo" (copy-sequence "foo") 100 100))
             '(completions-common-part
               nucleo-completion-high-score-face
               bold
               underline)))))

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
    (cl-letf (((symbol-function 'nucleo-completion-candidates)
               (lambda (_needle _candidates _ignore-case _by-length
                                _alphabetically _limit
                                &optional return-all-scores)
                 (nucleo-completion-tests--bundle
                  '(("foo" 100 (0 1)) ("fob" 50 (0 1)))
                  return-all-scores))))
      (let ((all (nucleo-completion-all-completions
                  "fo" '("foo" "fob" "bar"))))
        (should (equal (nucleo-completion-tests--plain all)
                       '("foo" "fob")))
        (should (nucleo-completion-tests--high-score-face-p
                 (get-text-property 0 'face (car all))))
        (should (memq 'nucleo-completion-low-score-face
                      (ensure-list (get-text-property 0 'face (cadr all)))))))))

(ert-deftest nucleo-completion-skips-highlight-tables-when-unused-test ()
  (let ((completion-ignore-case nil)
        (nucleo-completion-max-highlighted-completions 0)
        (completion-lazy-hilit nil))
    (cl-letf (((symbol-function 'nucleo-completion-candidates)
               (lambda (_needle _candidates _ignore-case _by-length
                                _alphabetically _limit
                                &optional return-all-scores)
                 (nucleo-completion-tests--bundle
                  '(("foo" 100 (0 1)) ("fob" 50 (0 1)))
                  return-all-scores)))
              ((symbol-function 'nucleo-completion--top-info-hash)
               (lambda (_top-info)
                 (error "top-info hash must not be built")))
              ((symbol-function 'nucleo-completion--full-scores-hash)
               (lambda (_candidates _full-scores)
                 (error "full-scores hash must not be built"))))
      (should (equal (nucleo-completion-tests--plain
                      (nucleo-completion-all-completions
                       "fo" '("foo" "fob" "bar")))
                     '("foo" "fob"))))))

(ert-deftest nucleo-completion-lazy-highlight-avoids-key-allocation-test ()
  "Hash tables keyed on candidate strings rely on the `equal'
test, which already compares string contents independently of
text properties.  The lazy highlight lambda must therefore avoid
allocating a stripped key with `substring-no-properties' on each
invocation."
  (let ((completion-ignore-case nil)
        (completion-lazy-hilit t)
        (nucleo-completion-max-highlighted-completions 10)
        (calls 0))
    (defvar completion-lazy-hilit-fn)
    (cl-letf (((symbol-function 'nucleo-completion-candidates)
               (lambda (_needle _candidates _ignore-case _by-length
                                _alphabetically _limit
                                &optional return-all-scores)
                 (nucleo-completion-tests--bundle '(("foo" 100 (0 1)))
                                                  return-all-scores)))
              ((symbol-function 'substring-no-properties)
               (lambda (string &optional start end)
                 (setq calls (1+ calls))
                 (if (or start end)
                     (substring string (or start 0) end)
                   (copy-sequence string)))))
      (nucleo-completion-all-completions "fo" '("foo" "bar"))
      (setq calls 0)
      (funcall completion-lazy-hilit-fn (copy-sequence "foo"))
      (should (= calls 0)))))

(ert-deftest nucleo-completion-regexp-only-match-is-high-score-highlighted-test ()
  (let ((completion-ignore-case nil)
        (nucleo-completion-highlight-score-bands t)
        (nucleo-completion-high-score-ratio 0.85)
        (nucleo-completion-max-highlighted-completions 10)
        (nucleo-completion-regexp-functions
         (list (lambda (term)
                 (when (string= term "nihon")
                   "日本")))))
    (cl-letf (((symbol-function 'nucleo-completion-candidates)
               (lambda (_needle candidates _ignore-case _by-length
                                _alphabetically _limit
                                &optional return-all-scores)
                 (nucleo-completion-tests--bundle
                  (cl-loop for candidate in candidates
                           when (string= candidate "roman-nihon")
                           collect (list candidate 128 '(0 1)))
                  return-all-scores))))
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
        (should (equal plain '("日本語" "roman-nihon")))
        (should (memq 'completions-common-part regexp-only-faces))
        (should (nucleo-completion-tests--high-score-face-p regexp-only-faces))
        (should-not (memq 'nucleo-completion-low-score-face regexp-only-faces))
        (should (nucleo-completion-tests--high-score-face-p module-faces))))))

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
