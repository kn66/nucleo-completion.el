;;; nucleo-completion-tests.el --- Tests for nucleo-completion  -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(setq load-prefer-newer t)
(eval-and-compile
  (let ((file (or load-file-name
                  (and (boundp 'byte-compile-current-file)
                       byte-compile-current-file)
                  buffer-file-name)))
    (when file
      (add-to-list 'load-path
                   (file-name-directory
                    (directory-file-name
                     (file-name-directory file)))))))
(require 'nucleo-completion)

;; Existing tests opt into expansion explicitly.
(setq-default nucleo-completion-languages nil)

;;; Code:

(defvar completion-lazy-hilit-fn nil
  "Function used by completion UIs to lazily highlight candidates.")

(defvar nucleo-completion-tests-history nil
  "History variable used by nucleo-completion tests.")

(defvar corfu-history nil
  "Mock Corfu history list used by nucleo-completion tests.")

(defvar corfu-history-mode nil
  "Mock Corfu history mode state used by nucleo-completion tests.")

(defun nucleo-completion-tests--plain (strings)
  "Return STRINGS without text properties."
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

(defun nucleo-completion-tests--context (table &optional pred regexp-list)
  "Return an interrupt-reuse context for TABLE, PRED, and REGEXP-LIST."
  (nucleo-completion--completion-context table pred regexp-list))

(defun nucleo-completion-tests--numbered-a-candidates (count)
  "Return COUNT candidates that all match the pattern \"a\"."
  (cl-loop for i below count
           collect (format "a%03d" i)))

(defmacro nucleo-completion-tests--with-mock-candidates (triples &rest body)
  "Stub `nucleo-completion-candidates' in BODY.
The stub returns TRIPLES wrapped as a module-result bundle."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'nucleo-completion-candidates)
              (lambda (_needle _candidates _ignore-case _by-length
                               _alphabetically _limit
                               &optional return-all-scores)
                (nucleo-completion-tests--bundle ,triples
                                                 return-all-scores))))
     ,@body))

(defun nucleo-completion-tests--high-score-face-p (faces)
  "Return non-nil when FACES include the high-score face."
  (cl-some (lambda (face)
             (eq face 'nucleo-completion-high-score-face))
           (ensure-list faces)))

(defvar nucleo-completion-tests--regexp-calls 0
  "Number of calls made to `nucleo-completion-tests--nihon-regexp'.")

(defun nucleo-completion-tests--nihon-regexp (term)
  "Return a Japanese regexp for TERM when it is the romanized test term."
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
         (nucleo-completion-module-directory "/tmp/nucleo-modules/")
         (nucleo-completion-required-module-version "9.8.7")
         (system-type 'gnu/linux)
         (system-configuration "x86_64-pc-linux-gnu")
         (candidates (nucleo-completion--module-candidates)))
    (should (member (expand-file-name
                     "v9.8.7/x86_64-unknown-linux-gnu/libnucleo_completion_module.so"
                     nucleo-completion-module-directory)
                    candidates))
    (should (member (expand-file-name
                     "bin/x86_64-unknown-linux-gnu/libnucleo_completion_module.so"
                     nucleo-completion--directory)
                    candidates))
    (should (member (expand-file-name
                     "bin/x86_64-unknown-linux-musl/libnucleo_completion_module.so"
                     nucleo-completion--directory)
                    candidates))
    (should (member (expand-file-name
                     "target/release/libnucleo_completion_module.so"
                     nucleo-completion--directory)
                    candidates))
    (should (member (expand-file-name
                     "target/debug/libnucleo_completion_module.so"
                     nucleo-completion--directory)
                    candidates))))

(ert-deftest nucleo-completion-module-candidates-prefers-versioned-paths-test ()
  (let* ((nucleo-completion--directory "/tmp/nucleo-completion/")
         (nucleo-completion-module-directory "/tmp/nucleo-modules/")
         (nucleo-completion-required-module-version "9.8.7")
         (system-type 'gnu/linux)
         (system-configuration "x86_64-pc-linux-gnu")
         (versioned
          (expand-file-name
           "v9.8.7/x86_64-unknown-linux-gnu/libnucleo_completion_module.so"
           nucleo-completion-module-directory))
         (target
          (expand-file-name
           "target/release/libnucleo_completion_module.so"
           nucleo-completion--directory))
         (generic "/tmp/old-load-path/libnucleo_completion_module.so"))
    (cl-letf (((symbol-function 'locate-library)
               (lambda (_library &rest _args)
                 generic)))
      (let ((candidates (nucleo-completion--module-candidates)))
        (should (< (cl-position versioned candidates :test #'equal)
                   (cl-position generic candidates :test #'equal)))
        (should (< (cl-position target candidates :test #'equal)
                   (cl-position generic candidates :test #'equal)))))))

(ert-deftest nucleo-completion-module-install-triple-test ()
  (let ((system-type 'windows-nt)
        (system-configuration "x86_64-w64-mingw32"))
    (should (equal (nucleo-completion--module-install-triple)
                   "x86_64-pc-windows-msvc")))
  (let ((system-type 'gnu/linux)
        (system-configuration "aarch64-unknown-linux-gnu"))
    (should-not (nucleo-completion--module-install-triple))))

(ert-deftest nucleo-completion-module-release-directory-name-test ()
  (let ((nucleo-completion-module-release-tag nil)
        (nucleo-completion-required-module-version "9.8.7"))
    (should (equal (nucleo-completion--module-release-directory-name)
                   "v9.8.7")))
  (let ((nucleo-completion-module-release-tag 'latest)
        (nucleo-completion-required-module-version "9.8.7"))
    (should (equal (nucleo-completion--module-release-directory-name)
                   "latest")))
  (let ((nucleo-completion-module-release-tag "v1.2.3")
        (nucleo-completion-required-module-version "9.8.7"))
    (should (equal (nucleo-completion--module-release-directory-name)
                   "v1.2.3"))))

(ert-deftest nucleo-completion-module-asset-url-test ()
  (let ((system-type 'gnu/linux)
        (nucleo-completion-module-release-repository "example/repo")
        (nucleo-completion-module-release-tag nil)
        (nucleo-completion-required-module-version "9.8.7"))
    (should (equal
             (nucleo-completion--module-asset-url
              "x86_64-unknown-linux-gnu")
             "https://github.com/example/repo/releases/download/v9.8.7/nucleo-completion-module-x86_64-unknown-linux-gnu.so"))
    (should (equal
             (nucleo-completion--module-asset-url
              "x86_64-unknown-linux-gnu" t)
             "https://github.com/example/repo/releases/download/v9.8.7/nucleo-completion-module-x86_64-unknown-linux-gnu.so.sha256")))
  (let ((system-type 'gnu/linux)
        (nucleo-completion-module-release-repository "example/repo")
        (nucleo-completion-module-release-tag 'latest))
    (should (equal
             (nucleo-completion--module-asset-url
              "x86_64-unknown-linux-gnu")
             "https://github.com/example/repo/releases/latest/download/nucleo-completion-module-x86_64-unknown-linux-gnu.so")))
  (let ((system-type 'darwin)
        (nucleo-completion-module-release-repository "example/repo")
        (nucleo-completion-module-release-tag "v1.2.3"))
    (should (equal
             (nucleo-completion--module-asset-url
              "aarch64-apple-darwin")
             "https://github.com/example/repo/releases/download/v1.2.3/nucleo-completion-module-aarch64-apple-darwin.dylib"))))

(ert-deftest nucleo-completion-stale-installed-module-directories-test ()
  (let* ((root (make-temp-file "nucleo-completion-test-" t))
         (nucleo-completion-module-directory
          (expand-file-name "modules" root))
         (nucleo-completion-required-module-version "9.8.7")
         (system-type 'gnu/linux)
         (system-configuration "x86_64-pc-linux-gnu")
         (library "libnucleo_completion_module.so")
         (current-dir
          (expand-file-name
           "v9.8.7/x86_64-unknown-linux-gnu"
           nucleo-completion-module-directory))
         (old-dir
          (expand-file-name
           "v9.8.6/x86_64-unknown-linux-gnu"
           nucleo-completion-module-directory))
         (latest-dir
          (expand-file-name
           "latest/x86_64-unknown-linux-gnu"
           nucleo-completion-module-directory)))
    (unwind-protect
        (progn
          (dolist (dir (list current-dir old-dir latest-dir))
            (make-directory dir t)
            (with-temp-file (expand-file-name library dir)
              (insert "module")))
          (let* ((digest (secure-hash 'sha256 "module"))
                 (selected (concat "libnucleo_completion_module-" digest ".so")))
            (rename-file (expand-file-name library old-dir)
                         (expand-file-name selected old-dir))
            (with-temp-file (expand-file-name "current" old-dir)
              (insert digest "\n")))
          (let ((dirs (nucleo-completion--stale-installed-module-directories)))
            (should (member old-dir dirs))
            (should (member latest-dir dirs))
            (should-not (member current-dir dirs))))
      (delete-directory root t))))

(ert-deftest nucleo-completion-download-file-reports-http-error-test ()
  (let ((buffer (generate-new-buffer " *nucleo-completion-http-error*"))
        (destination (make-temp-file "nucleo-completion-download-")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local url-http-response-status 404)
            (insert "Not found"))
          (cl-letf (((symbol-function 'url-retrieve-synchronously)
                     (lambda (_url &rest _args)
                       buffer)))
            (should-error
             (nucleo-completion--download-file
              "https://example.invalid/missing.sha256" destination)
             :type 'error)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when (file-exists-p destination)
        (delete-file destination)))))

(ert-deftest nucleo-completion-install-module-downloads-and-verifies-test ()
  (let* ((root (make-temp-file "nucleo-completion-test-" t))
         (nucleo-completion-module-directory
          (expand-file-name "modules" root))
         (system-type 'gnu/linux)
         (system-configuration "x86_64-pc-linux-gnu")
         (payload "module contents")
         loaded
         (original-featurep (symbol-function 'featurep))
         downloads)
    (unwind-protect
        (cl-letf (((symbol-function 'nucleo-completion--download-file)
                   (lambda (url file)
                     (push url downloads)
                     (with-temp-file file
                       (insert
                        (if (string-suffix-p ".sha256" url)
                            (concat (secure-hash 'sha256 payload) "  asset\n")
                          payload)))))
                  ((symbol-function 'nucleo-completion--dynamic-modules-supported-p)
                   (lambda () t))
                  ((symbol-function 'nucleo-completion--load-module)
                   (lambda ()
                     (setq loaded t)))
                  ((symbol-function 'nucleo-completion--module-ready-p)
                   (lambda () loaded))
                  ((symbol-function 'featurep)
                   (lambda (feature &optional subfeature)
                     (and (not (eq feature 'nucleo-completion-module))
                          (funcall original-featurep feature subfeature))))
                  ((symbol-function 'yes-or-no-p)
                   (lambda (_prompt)
                     (error "Install test should not prompt"))))
          (let ((destination (nucleo-completion-install-module nil t)))
            (should (equal (file-name-nondirectory destination)
                           (concat "libnucleo_completion_module-"
                                   (secure-hash 'sha256 payload) ".so")))
            (should (equal (with-temp-buffer
                             (insert-file-contents destination)
                             (buffer-string))
                           payload))
            (should (= (length downloads) 2))))
      (delete-directory root t))))

(ert-deftest nucleo-completion-install-module-updates-loaded-module-for-restart-test ()
  (let* ((root (make-temp-file "nucleo-completion-test-" t))
         (nucleo-completion-module-directory
          (expand-file-name "modules" root))
         (nucleo-completion-required-module-version "9.8.7")
         (system-type 'gnu/linux)
         (system-configuration "x86_64-pc-linux-gnu")
         (payload "new module contents")
         (directory
          (expand-file-name
           "v9.8.7/x86_64-unknown-linux-gnu"
           nucleo-completion-module-directory))
         (old-destination
          (expand-file-name "libnucleo_completion_module.so" directory))
         (destination
          (expand-file-name (concat "libnucleo_completion_module-"
                                    (secure-hash 'sha256 payload) ".so") directory))
         downloads
         messages)
    (unwind-protect
        (progn
          (make-directory directory t)
          (with-temp-file old-destination
            (insert "old module contents"))
          (cl-letf (((symbol-function 'nucleo-completion--download-file)
                     (lambda (url file)
                       (push url downloads)
                       (with-temp-file file
                         (insert
                          (if (string-suffix-p ".sha256" url)
                              (concat (secure-hash 'sha256 payload) "  asset\n")
                            payload)))))
                    ((symbol-function 'nucleo-completion--dynamic-modules-supported-p)
                     (lambda () t))
                    ((symbol-function 'nucleo-completion--load-module)
                     (lambda ()
                       (error "Loaded module should not be reloaded")))
                    ((symbol-function 'nucleo-completion--module-ready-p)
                     (lambda () t))
                    ((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (push (apply #'format format-string args) messages))))
            (should (equal (nucleo-completion-install-module t t)
                           destination))
            (should (equal (with-temp-buffer
                             (insert-file-contents destination)
                             (buffer-string))
                           payload))
            (should (equal (with-temp-buffer
                             (insert-file-contents old-destination)
                             (buffer-string))
                           "old module contents"))
            (should (= (length downloads) 2))
            (should (string-match-p
                     "restart Emacs"
                     (car messages)))))
      (delete-directory root t))))

(defmacro nucleo-completion-tests--with-module-install (&rest body)
  "Run BODY with downloads and module loading isolated in a temporary directory."
  (declare (indent 0))
  `(let* ((root (make-temp-file "nucleo-completion-install-test-" t))
          (nucleo-completion-module-directory (expand-file-name "modules" root))
          (nucleo-completion-required-module-version "9.8.7")
          (system-type 'gnu/linux)
          (system-configuration "x86_64-pc-linux-gnu")
          (directory (expand-file-name "v9.8.7/x86_64-unknown-linux-gnu"
                                       nucleo-completion-module-directory))
          (library "libnucleo_completion_module.so")
          (payload "first module")
          module-download)
     (unwind-protect
         (cl-letf (((symbol-function 'nucleo-completion--dynamic-modules-supported-p)
                    (lambda () t))
                   ((symbol-function 'nucleo-completion--module-ready-p) (lambda () t))
                   ((symbol-function 'nucleo-completion--download-file)
                    (lambda (url file)
                      (unless (string-suffix-p ".sha256" url)
                        (setq module-download file))
                      (with-temp-file file
                        (insert (if (string-suffix-p ".sha256" url)
                                    (concat (secure-hash 'sha256 payload) "  asset\n")
                                  payload))))))
           ,@body)
       (delete-directory root t))))

(ert-deftest nucleo-completion-install-module-reuses-immutable-digest-test ()
  (nucleo-completion-tests--with-module-install
    (let* ((first (nucleo-completion-install-module t t))
           (original-rename (symbol-function 'rename-file)))
      (should (equal (file-name-directory module-download)
                     (file-name-as-directory directory)))
      (cl-letf (((symbol-function 'rename-file)
                 (lambda (from to &optional overwrite)
                   (when (equal to first)
                     (error "Cannot overwrite a loaded module"))
                   (funcall original-rename from to overwrite))))
        (should (equal (nucleo-completion-install-module t t) first)))
      (should (equal (with-temp-buffer (insert-file-contents first) (buffer-string))
                     payload)))))

(ert-deftest nucleo-completion-install-module-retains-old-digest-test ()
  (nucleo-completion-tests--with-module-install
    (let ((first (nucleo-completion-install-module t t)))
      (setq payload "second module")
      (let ((second (nucleo-completion-install-module t t)))
        (should-not (equal first second))
        (should (equal (with-temp-buffer (insert-file-contents first) (buffer-string))
                       "first module"))
        (should (equal (with-temp-buffer (insert-file-contents second) (buffer-string))
                       payload))
        (should (equal (nucleo-completion--module-file-in-directory directory library)
                       second))))))

(ert-deftest nucleo-completion-install-module-rejects-corrupt-digest-file-test ()
  (nucleo-completion-tests--with-module-install
    (let ((first (nucleo-completion-install-module t t)))
      (setq payload "second module")
      (let ((conflict (expand-file-name
                       (concat "libnucleo_completion_module-"
                               (secure-hash 'sha256 payload) ".so") directory)))
        (with-temp-file conflict (insert "not the expected module"))
        (should-error (nucleo-completion-install-module t t))
        (should (equal (with-temp-buffer (insert-file-contents conflict)
                                         (buffer-string))
                       "not the expected module"))
        (should (equal (nucleo-completion--module-file-in-directory directory library)
                       first))))))

(ert-deftest nucleo-completion-install-module-selector-failure-preserves-selection-test ()
  (nucleo-completion-tests--with-module-install
    (let ((first (nucleo-completion-install-module t t))
          (original-rename (symbol-function 'rename-file)))
      (setq payload "second module")
      (cl-letf (((symbol-function 'rename-file)
                 (lambda (from to &optional overwrite)
                   (if (equal to (expand-file-name "current" directory))
                       (error "Simulated selector update failure")
                     (funcall original-rename from to overwrite)))))
        (should-error (nucleo-completion-install-module t t)))
      (should (equal (nucleo-completion--module-file-in-directory directory library)
                     first))
      (should-not (file-exists-p module-download))
      (should (equal (sort (cl-remove-if (lambda (file) (member file '("." "..")))
                                    (directory-files directory nil nil t))
                           #'string<)
                     (sort (list "current" (file-name-nondirectory first)
                                 (concat "libnucleo_completion_module-"
                                         (secure-hash 'sha256 payload) ".so"))
                           #'string<))))))

(ert-deftest nucleo-completion-install-module-incompatible-loaded-module-test ()
  (nucleo-completion-tests--with-module-install
    (let ((original-featurep (symbol-function 'featurep)) messages)
      (cl-letf (((symbol-function 'nucleo-completion--module-ready-p) (lambda () nil))
                ((symbol-function 'featurep)
                 (lambda (feature &optional subfeature)
                   (or (eq feature 'nucleo-completion-module)
                       (funcall original-featurep feature subfeature))))
                ((symbol-function 'nucleo-completion--load-module)
                 (lambda () (error "A loaded incompatible module cannot be reloaded")))
                ((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args) messages))))
        (should (file-exists-p (nucleo-completion-install-module t t)))
        (should (string-match-p "restart Emacs" (car messages)))))))

(ert-deftest nucleo-completion-install-module-bad-checksum-preserves-selection-test ()
  (nucleo-completion-tests--with-module-install
    (let ((first (nucleo-completion-install-module t t)))
      (cl-letf (((symbol-function 'nucleo-completion--download-file)
                 (lambda (url file)
                   (unless (string-suffix-p ".sha256" url)
                     (setq module-download file))
                   (with-temp-file file
                     (insert (if (string-suffix-p ".sha256" url)
                                 (concat (make-string 64 ?0) "  asset\n")
                               "corrupt download"))))))
        (should-error (nucleo-completion-install-module t t)))
      (should-not (file-exists-p module-download))
      (should (equal (nucleo-completion--module-file-in-directory directory library)
                     first))
      (should (equal (sort (cl-remove-if (lambda (file) (member file '("." "..")))
                                        (directory-files directory nil nil t))
                           #'string<)
                     (sort (list "current" (file-name-nondirectory first))
                           #'string<))))))

(ert-deftest nucleo-completion-module-selector-falls-back-to-legacy-test ()
  (let* ((directory (make-temp-file "nucleo-selector-test-" t))
         (library "libnucleo_completion_module.so")
         (legacy (expand-file-name library directory))
         (selector (expand-file-name "current" directory)))
    (unwind-protect
        (progn
          (with-temp-file legacy (insert "legacy"))
          (dolist (contents (list nil "bad\n" (concat (make-string 64 ?a) "\n")
                                  (concat (make-string 64 ?A) "\n")))
            (if contents (with-temp-file selector (insert contents))
              (when (file-exists-p selector) (delete-file selector)))
            (should (equal (nucleo-completion--module-file-in-directory directory library)
                           legacy))))
      (delete-directory directory t))))

(ert-deftest nucleo-completion-ensure-module-skips-ready-module-test ()
  (let (load-called install-called)
    (cl-letf (((symbol-function 'nucleo-completion--dynamic-modules-supported-p)
               (lambda () t))
              ((symbol-function 'nucleo-completion--module-ready-p)
               (lambda () t))
              ((symbol-function 'nucleo-completion--load-module)
               (lambda ()
                 (setq load-called t)))
              ((symbol-function 'nucleo-completion-install-module)
               (lambda (&rest _args)
                 (setq install-called t))))
      (should (eq (nucleo-completion-ensure-module) t))
      (should-not load-called)
      (should-not install-called))))

(ert-deftest nucleo-completion-ensure-module-loads-installed-module-test ()
  (let (install-called load-called
                       (ready nil))
    (cl-letf (((symbol-function 'nucleo-completion--dynamic-modules-supported-p)
               (lambda () t))
              ((symbol-function 'nucleo-completion--module-ready-p)
               (lambda () ready))
              ((symbol-function 'nucleo-completion--load-module)
               (lambda ()
                 (setq load-called t)
                 (setq ready t)))
              ((symbol-function 'nucleo-completion-install-module)
               (lambda (&rest _args)
                 (setq install-called t))))
      (should (eq (nucleo-completion-ensure-module) t))
      (should load-called)
      (should-not install-called))))

(ert-deftest nucleo-completion-ensure-module-installs-from-lisp-without-confirm-test ()
  (let ((ready nil)
        install-args)
    (cl-letf (((symbol-function 'nucleo-completion--dynamic-modules-supported-p)
               (lambda () t))
              ((symbol-function 'nucleo-completion--module-ready-p)
               (lambda () ready))
              ((symbol-function 'nucleo-completion--load-module)
               (lambda () nil))
              ((symbol-function 'nucleo-completion--current-installed-module-file)
               (lambda () nil))
              ((symbol-function 'nucleo-completion-install-module)
               (lambda (&optional force no-confirm)
                 (setq install-args (list force no-confirm))
                 (setq ready t)
                 "/tmp/nucleo-module")))
      (should (eq (nucleo-completion-ensure-module) t))
      (should (equal install-args '(nil t))))))

(ert-deftest nucleo-completion-ensure-module-interactive-install-confirms-test ()
  (let ((ready nil)
        install-args)
    (cl-letf (((symbol-function 'nucleo-completion--dynamic-modules-supported-p)
               (lambda () t))
              ((symbol-function 'nucleo-completion--module-ready-p)
               (lambda () ready))
              ((symbol-function 'nucleo-completion--load-module)
               (lambda () nil))
              ((symbol-function 'nucleo-completion-install-module)
               (lambda (&optional force no-confirm)
                 (setq install-args (list force no-confirm))
                 (setq ready t)
                 "/tmp/nucleo-module"))
              ((symbol-function 'called-interactively-p)
               (lambda (_kind) t)))
      (should (commandp 'nucleo-completion-ensure-module))
      (should (eq (nucleo-completion-ensure-module) t))
      (should (equal install-args '(nil nil))))))

(ert-deftest nucleo-completion-ensure-module-installs-stale-loaded-module-test ()
  (let ((install-args nil))
    (cl-letf (((symbol-function 'nucleo-completion--dynamic-modules-supported-p)
               (lambda () t))
              ((symbol-function 'nucleo-completion--module-ready-p)
               (lambda () t))
              ((symbol-function 'nucleo-completion--loaded-installed-module-stale-p)
               (lambda () t))
              ((symbol-function 'nucleo-completion--load-module)
               (lambda ()
                 (error "Loaded stale module should not be loaded again")))
              ((symbol-function 'nucleo-completion--current-installed-module-file)
               (lambda () nil))
              ((symbol-function 'nucleo-completion-install-module)
               (lambda (&optional force no-confirm)
                 (setq install-args (list force no-confirm))
                 "/tmp/nucleo-module")))
      (should (eq (nucleo-completion-ensure-module) t))
      (should (equal install-args '(nil t))))))

(ert-deftest nucleo-completion-maybe-prompt-module-install-test ()
  (let ((nucleo-completion-module-install-policy 'prompt)
        (nucleo-completion--module-install-prompted nil)
        called)
    (cl-letf (((symbol-function 'nucleo-completion--module-ready-p)
               (lambda () nil))
              ((symbol-function 'nucleo-completion--module-install-triple)
               (lambda () "x86_64-unknown-linux-gnu"))
              ((symbol-function 'nucleo-completion-install-module)
               (lambda (&rest _args)
                 (setq called t))))
      (let ((noninteractive nil))
        (nucleo-completion--maybe-prompt-module-install))
      (should called)
      (should nucleo-completion--module-install-prompted))))

(ert-deftest nucleo-completion-maybe-prompt-skips-explicit-install-command-test ()
  (let ((nucleo-completion-module-install-policy 'prompt)
        (nucleo-completion--module-install-prompted nil)
        called)
    (cl-letf (((symbol-function 'nucleo-completion--module-ready-p)
               (lambda () nil))
              ((symbol-function 'nucleo-completion--module-install-triple)
               (lambda () "x86_64-unknown-linux-gnu"))
              ((symbol-function 'nucleo-completion-install-module)
               (lambda (&rest _args)
                 (setq called t))))
      (let ((noninteractive nil)
            (this-command 'nucleo-completion-install-module))
        (nucleo-completion--maybe-prompt-module-install))
      (should-not called)
      (should-not nucleo-completion--module-install-prompted))))

(ert-deftest nucleo-completion-no-dynamic-module-support-test ()
  (let ((nucleo-completion-module-load-errors 'stale))
    (cl-letf (((symbol-function 'nucleo-completion--dynamic-modules-supported-p)
               (lambda () nil))
              ((symbol-function 'module-load)
               (lambda (_file)
                 (error "Module-load must not be called"))))
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
                 (error "Cannot load %s" file))))
      (nucleo-completion--load-module)
      (should (equal nucleo-completion-module-load-errors
                     '(("/tmp/nucleo-a.so" . "Cannot load /tmp/nucleo-a.so")
                       ("/tmp/nucleo-b.so" . "Cannot load /tmp/nucleo-b.so")))))))

(ert-deftest nucleo-completion-load-module-warns-stale-installed-module-test ()
  (let* ((root (make-temp-file "nucleo-completion-test-" t))
         (nucleo-completion-module-directory
          (expand-file-name "modules" root))
         (nucleo-completion-required-module-version "9.8.7")
         (nucleo-completion--loaded-module-file nil)
         (nucleo-completion--stale-module-warning-shown nil)
         (nucleo-completion--module-version-warning-shown nil)
         (old-file
          (expand-file-name
           "v9.8.6/x86_64-unknown-linux-gnu/libnucleo_completion_module.so"
           nucleo-completion-module-directory))
         warnings
         (original-featurep (symbol-function 'featurep)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory old-file) t)
          (with-temp-file old-file
            (insert "module"))
          (cl-letf (((symbol-function 'featurep)
                     (lambda (feature &optional subfeature)
                       (if (eq feature 'nucleo-completion-module)
                           nil
                         (funcall original-featurep feature subfeature))))
                    ((symbol-function 'nucleo-completion--module-candidates)
                     (lambda () (list old-file)))
                    ((symbol-function 'module-load)
                     (lambda (_file) t))
                    ((symbol-function 'nucleo-completion-module-version)
                     (lambda () "9.8.7"))
                    ((symbol-function 'display-warning)
                     (lambda (type message &optional level buffer-name)
                       (push (list type message level buffer-name) warnings))))
            (nucleo-completion--load-module)
            (should (equal nucleo-completion--loaded-module-file old-file))
            (should (cl-some
                     (lambda (warning)
                       (string-match-p "expects module release v9.8.7"
                                       (cadr warning)))
                     warnings))))
      (delete-directory root t))))

(ert-deftest nucleo-completion-custom-group-test ()
  (let ((members (get 'nucleo-completion 'custom-group)))
    (dolist (symbol '(nucleo-completion-max-highlighted-completions
                      nucleo-completion-regexp-functions
                      nucleo-completion-report-regexp-function-errors
                      nucleo-completion-regexp-minimum-term-length
                      nucleo-completion-regexp-only-match-priority
                      nucleo-completion-scrub-non-unicode-candidates
                      nucleo-completion-sort-ties-by-history
                      nucleo-completion-sort-ties-by-length
                      nucleo-completion-sort-ties-alphabetically
                      nucleo-completion-highlight-score-bands
                      nucleo-completion-high-score-ratio
                      nucleo-completion-high-score-emphasis
                      nucleo-completion-report-module-load-errors
                      nucleo-completion-module-directory
                      nucleo-completion-module-release-repository
                      nucleo-completion-module-release-tag
                      nucleo-completion-module-install-policy))
      (should (member (list symbol 'custom-variable) members)))))

(ert-deftest nucleo-completion-lazy-highlight-variable-is-special-test ()
  (should (special-variable-p 'completion-lazy-hilit)))

(ert-deftest nucleo-completion-removed-optimization-options-test ()
  (dolist (symbol '(nucleo-completion-persistent-regexp-cache-size
                    nucleo-completion-long-candidate-threshold
                    nucleo-completion-long-candidate-regexp-threshold
                    nucleo-completion-long-candidate-highlight-threshold))
    (should-not (custom-variable-p symbol))))

(ert-deftest nucleo-completion-highlight-limit-sanitizes-test ()
  (let ((nucleo-completion-max-highlighted-completions 3))
    (should (= (nucleo-completion--highlight-limit) 3)))
  (let ((nucleo-completion-max-highlighted-completions -1))
    (should (= (nucleo-completion--highlight-limit) 0)))
  (let ((nucleo-completion-max-highlighted-completions 'invalid))
    (should (= (nucleo-completion--highlight-limit) 0))))

(ert-deftest nucleo-completion-regexp-minimum-term-length-sanitizes-test ()
  (let ((nucleo-completion-regexp-minimum-term-length 3))
    (should (= (nucleo-completion--regexp-minimum-term-length) 3)))
  (let ((nucleo-completion-regexp-minimum-term-length -1))
    (should (= (nucleo-completion--regexp-minimum-term-length) 2)))
  (let ((nucleo-completion-regexp-minimum-term-length 'invalid))
    (should (= (nucleo-completion--regexp-minimum-term-length) 2))))

(ert-deftest nucleo-completion-regexp-only-match-priority-test ()
  (let ((nucleo-completion-regexp-only-match-priority 'non-ascii))
    (should (nucleo-completion--promote-regexp-only-candidate-p "日本語"))
    (should-not
     (nucleo-completion--promote-regexp-only-candidate-p "org-unrelated")))
  (let ((nucleo-completion-regexp-only-match-priority 'before))
    (should
     (nucleo-completion--promote-regexp-only-candidate-p "org-unrelated")))
  (let ((nucleo-completion-regexp-only-match-priority 'after))
    (should-not (nucleo-completion--promote-regexp-only-candidate-p "日本語"))))

(ert-deftest nucleo-completion-regexp-only-priority-ignores-tofu-test ()
  "Internal disambiguation characters must not make ASCII candidates prominent."
  (let ((nucleo-completion-regexp-only-match-priority 'non-ascii)
        (tofu (string #x200000)))
    (should-not
     (nucleo-completion--promote-regexp-only-candidate-p
      (concat "org-unrelated" tofu)))
    (should
     (nucleo-completion--promote-regexp-only-candidate-p
      (concat "日本語" tofu)))))

(ert-deftest nucleo-completion-regexp-only-split-skips-fixed-priority-test ()
  (dolist (priority '(before after))
    (let ((nucleo-completion-regexp-only-match-priority priority)
          (candidates '("日本語" "org-readable")))
      (cl-letf (((symbol-function
                  'nucleo-completion--promote-regexp-only-candidate-p)
                 (lambda (_candidate)
                   (error "Fixed regexp-only priority needs no split"))))
        (pcase-let ((`(,before . ,after)
                     (nucleo-completion--split-regexp-only-candidates
                      candidates)))
          (if (eq priority 'before)
              (progn
                (should (eq before candidates))
                (should-not after))
            (should-not before)
            (should (eq after candidates))))))))

(ert-deftest nucleo-completion-high-score-ratio-sanitizes-test ()
  (let ((nucleo-completion-high-score-ratio 0.5))
    (should (= (nucleo-completion--high-score-ratio) 0.5)))
  (let ((nucleo-completion-high-score-ratio -1))
    (should (= (nucleo-completion--high-score-ratio) 0.0)))
  (let ((nucleo-completion-high-score-ratio 2))
    (should (= (nucleo-completion--high-score-ratio) 1.0)))
  (let ((nucleo-completion-high-score-ratio 'invalid))
    (should (= (nucleo-completion--high-score-ratio) 0.85))))

(ert-deftest nucleo-completion-high-score-skips-exact-word-for-high-score-test ()
  (let ((nucleo-completion-high-score-ratio 0.85))
    (cl-letf (((symbol-function 'nucleo-completion--exact-word-match-p)
               (lambda (&rest _)
                 (error "High scores need no exact-word regexp"))))
      (should (nucleo-completion--high-score-p
               "needle" "candidate" 90 100)))))

(ert-deftest nucleo-completion-high-score-exact-word-tolerates-missing-score-test ()
  (let ((nucleo-completion-high-score-ratio 0.85))
    (should (nucleo-completion--high-score-p "foo" "foo-bar" nil 100))
    (should (nucleo-completion--high-score-p "foo" "foo-bar" 'bad 100))))

(ert-deftest nucleo-completion-exact-word-regexps-cache-test ()
  (let ((nucleo-completion--exact-word-regexp-cache
         (make-hash-table :test #'equal)))
    (should (eq (nucleo-completion--exact-word-regexps "foo bar")
                (nucleo-completion--exact-word-regexps "foo bar")))))

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

(ert-deftest nucleo-completion-scrub-candidates-keeps-properties-when-disabled-test ()
  (let* ((candidate (propertize "foo" 'consult-location 'marker))
         (candidates (list candidate))
         (nucleo-completion-scrub-non-unicode-candidates nil)
         (nucleo-completion--force-scrub-non-unicode-candidates nil)
         (result (nucleo-completion--scrub-candidates candidates)))
    (should (eq (car result) candidates))
    (should-not (cdr result))
    (should (eq (caar result) candidate))))

(ert-deftest nucleo-completion-scrub-candidates-strips-non-unicode-test ()
  (let* ((tofu (string #x200002))
         (consult-cand (concat "abc-def" tofu))
         (nucleo-completion-scrub-non-unicode-candidates t)
         (result (nucleo-completion--scrub-candidates
                  (list "foo" consult-cand)))
         (cleaned (car result))
         (map (cdr result)))
    (should (equal cleaned '("foo" "abc-def")))
    (should map)
    (should (eq (gethash (cadr cleaned) map) consult-cand))))

(ert-deftest nucleo-completion-scrub-candidates-skips-size-scan-test ()
  (let ((tofu (string #x200002))
        (nucleo-completion-scrub-non-unicode-candidates t)
        (original-make-hash-table (symbol-function 'make-hash-table)))
    (cl-letf (((symbol-function 'make-hash-table)
               (lambda (&rest args)
                 (should-not (memq :size args))
                 (apply original-make-hash-table args))))
      (should (cdr (nucleo-completion--scrub-candidates
                    (list "foo" (concat "bar" tofu))))))))

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
                      "abc" (list "foo" consult-cand) nil 0))
             (returned (nucleo-completion--bundle-candidates bundle))
             (top-info (nucleo-completion--bundle-top-info bundle)))
        (should (equal received-input '("foo" "abc-def")))
        (should (eq (cadr returned) consult-cand))
        (should (eq (car (cadr top-info)) consult-cand))))))

(ert-deftest nucleo-completion-module-results-keeps-scores-after-scrub-test ()
  "Restored scrubbed candidates keep module score text properties."
  (let* ((tofu (string #x200009))
         (consult-cand (concat "abc-def" tofu))
         (nucleo-completion-scrub-non-unicode-candidates t))
    (cl-letf (((symbol-function 'nucleo-completion-candidates)
               (lambda (_needle candidates _ignore-case _by-length
                                _alphabetically _limit
                                &optional return-all-scores)
                 (list (if return-all-scores
                           (list (nucleo-completion--propertize-score
                                  (car candidates) 100)
                                 (nucleo-completion--propertize-score
                                  (cadr candidates) 50))
                         candidates)
                       nil
                       (when return-all-scores '(100 50))))))
      (let* ((bundle (nucleo-completion--module-results
                      "abc" (list "foo" consult-cand) nil 0 t))
             (returned (nucleo-completion--bundle-candidates bundle)))
        (should (equal (mapcar #'substring-no-properties returned)
                       (list "foo" consult-cand)))
        (should (equal (nucleo-completion--candidate-score (car returned))
                       100))
        (should (equal (nucleo-completion--candidate-score (cadr returned))
                       50))))))

(ert-deftest nucleo-completion-module-results-skips-restore-after-interrupt-test ()
  "Interrupted scrubbed module calls skip candidate restoration work."
  (let* ((tofu (string #x20000c))
         (consult-cand (concat "abc-def" tofu))
         (nucleo-completion-scrub-non-unicode-candidates t))
    (cl-letf (((symbol-function 'nucleo-completion-candidates)
               (lambda (&rest _)
                 (list nucleo-completion--interrupted-sentinel nil nil)))
              ((symbol-function 'nucleo-completion--restore-bundle-candidates)
               (lambda (&rest _)
                 (error "interrupted bundles should not be restored"))))
      (should (nucleo-completion--bundle-interrupted-p
               (nucleo-completion--module-results
                "abc" (list consult-cand) nil 0))))))

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
                      "same" (list cand-a cand-b) nil 2))
             (returned (nucleo-completion--bundle-candidates bundle))
             (top-info (nucleo-completion--bundle-top-info bundle)))
        (should (equal received-input '("same" "same")))
        (should (eq (car returned) cand-a))
        (should (eq (cadr returned) cand-b))
        (should (eq (caar top-info) cand-a))
        (should (eq (caadr top-info) cand-b))))))

(ert-deftest nucleo-completion-module-results-restores-copied-scrubbed-candidates-test ()
  "Copied scrubbed candidates still restore to their originals."
  (let* ((tofu-a (string #x200007))
         (tofu-b (string #x200008))
         (cand-a (concat "same" tofu-a))
         (cand-b (concat "same" tofu-b))
         (nucleo-completion-scrub-non-unicode-candidates t))
    (cl-letf (((symbol-function 'nucleo-completion-candidates)
               (lambda (_needle candidates _ignore-case _by-length
                                _alphabetically _limit
                                &optional return-all-scores)
                 (let ((triples
                        (mapcar (lambda (candidate)
                                  (list (copy-sequence candidate) 100 nil))
                                candidates)))
                   (nucleo-completion-tests--bundle
                    triples return-all-scores)))))
      (let* ((bundle (nucleo-completion--module-results
                      "same" (list cand-a cand-b) nil 2))
             (returned (nucleo-completion--bundle-candidates bundle))
             (top-info (nucleo-completion--bundle-top-info bundle)))
        (should (eq (car returned) cand-a))
        (should (eq (cadr returned) cand-b))
        (should (eq (caar top-info) cand-a))
        (should (eq (caadr top-info) cand-b))))))

(ert-deftest nucleo-completion-module-results-restores-copied-scrub-collisions-test ()
  "Copied candidates restore correctly when clean and scrubbed text collide."
  (let* ((tofu (string #x20000a))
         (clean (propertize "same" 'nucleo-test-clean t))
         (scrubbed (concat "same" tofu))
         (nucleo-completion-scrub-non-unicode-candidates t))
    (cl-letf (((symbol-function 'nucleo-completion-candidates)
               (lambda (_needle candidates _ignore-case _by-length
                                _alphabetically _limit
                                &optional return-all-scores)
                 (let ((triples
                        (cl-loop for candidate in candidates
                                 for score in '(100 100)
                                 collect
                                 (list (nucleo-completion--propertize-score
                                        (copy-sequence candidate) score)
                                       score nil))))
                   (nucleo-completion-tests--bundle
                    triples return-all-scores)))))
      (let* ((bundle (nucleo-completion--module-results
                      "same" (list clean scrubbed) nil 2 t))
             (returned (nucleo-completion--bundle-candidates bundle))
             (top-info (nucleo-completion--bundle-top-info bundle)))
        (should (equal (mapcar #'substring-no-properties returned)
                       (list clean scrubbed)))
        (should (get-text-property 0 'nucleo-test-clean (car returned)))
        (should (equal (nucleo-completion--candidate-score (car returned))
                       100))
        (should (equal (nucleo-completion--candidate-score (cadr returned))
                       100))
        (should (eq (caar top-info) clean))
        (should (eq (caadr top-info) scrubbed))))))

(ert-deftest nucleo-completion-native-module-restores-reordered-scrub-collisions-test ()
  "Native module results keep identity when scrubbed collisions reorder."
  (unless (nucleo-completion--module-supports-history-p)
    (ert-skip "History-aware Rust module is not available"))
  (let* ((tofu (string #x20000b))
         (clean (propertize "same" 'nucleo-test-clean t))
         (scrubbed (concat "same" tofu))
         (nucleo-completion-scrub-non-unicode-candidates t)
         (bundle (nucleo-completion--module-results
                  "same" (list clean scrubbed) nil 2 t '(1 0)))
         (returned (nucleo-completion--bundle-candidates bundle))
         (top-info (nucleo-completion--bundle-top-info bundle)))
    (should (equal (mapcar #'substring-no-properties returned)
                   (list scrubbed clean)))
    (should-not (get-text-property 0 'nucleo-test-clean (car returned)))
    (should (get-text-property 0 'nucleo-test-clean (cadr returned)))
    (should (equal (nucleo-completion--candidate-score (car returned))
                   (car (nucleo-completion--bundle-full-scores bundle))))
    (should (equal (mapcar #'substring-no-properties
                           (mapcar #'nucleo-completion--top-info-candidate
                                   top-info))
                   (list scrubbed clean)))))

(ert-deftest nucleo-completion-module-results-skips-non-unicode-scan-by-default-test ()
  "Ordinary module calls avoid the per-candidate non-Unicode scan by default."
  (let ((nucleo-completion-scrub-non-unicode-candidates nil))
    (cl-letf (((symbol-function 'nucleo-completion--scrub-non-unicode-string)
               (lambda (_candidates)
                 (error "Non-Unicode scrub should not run")))
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

(ert-deftest nucleo-completion-module-results-retries-non-unicode-scrub-test ()
  "Module Unicode encoder failures enable scrub retry for that call."
  (let* ((tofu (string #x200006))
         (consult-cand (concat "\"日本太郎\" <taro@example.invalid>" tofu))
         (nucleo-completion-scrub-non-unicode-candidates nil)
         (nucleo-completion--force-scrub-non-unicode-candidates nil)
         calls received-inputs)
    (cl-letf (((symbol-function 'nucleo-completion-candidates)
               (lambda (_needle candidates _ignore-case _by-length
                                _alphabetically _limit
                                &optional return-all-scores)
                 (push candidates received-inputs)
                 (setq calls (1+ (or calls 0)))
                 (if (= calls 1)
                     (signal 'wrong-type-argument
                             (list 'unicode-string-p (car candidates)))
                   (nucleo-completion-tests--bundle
                    (mapcar (lambda (c) (list c 100 nil)) candidates)
                    return-all-scores)))))
      (let* ((bundle (nucleo-completion--module-results
                      "m" (list consult-cand) nil 1))
             (returned (nucleo-completion--bundle-candidates bundle)))
        (should (= calls 2))
        (should (equal (nreverse received-inputs)
                       (list (list consult-cand)
                             '("\"日本太郎\" <taro@example.invalid>"))))
        (should-not nucleo-completion--force-scrub-non-unicode-candidates)
        (should (eq (car returned) consult-cand))))))

(ert-deftest nucleo-completion-module-results-keeps-propertized-candidates-test ()
  "Propertized candidates are passed to the module unchanged."
  (let* ((consult-cand (propertize
                        "\"日本太郎\" <taro@example.invalid>"
                        'consult-location 'marker
                        'invisible t))
         (nucleo-completion-scrub-non-unicode-candidates nil)
         received-input)
    (cl-letf (((symbol-function 'nucleo-completion-candidates)
               (lambda (_needle candidates _ignore-case _by-length
                                _alphabetically _limit
                                &optional return-all-scores)
                 (setq received-input candidates)
                 (should (eq (car candidates) consult-cand))
                 (should (eq (get-text-property 0 'consult-location
                                                (car candidates))
                             'marker))
                 (nucleo-completion-tests--bundle
                  (mapcar (lambda (c) (list c 100 nil)) candidates)
                  return-all-scores))))
      (let* ((bundle (nucleo-completion--module-results
                      "aa" (list consult-cand) nil 1 t))
             (returned (nucleo-completion--bundle-candidates bundle))
             (top-info (nucleo-completion--bundle-top-info bundle)))
        (should received-input)
        (should (equal (substring-no-properties (car returned))
                       (substring-no-properties consult-cand)))
        (should (equal (nucleo-completion--candidate-score (car returned))
                       100))
        (should (eq (caar top-info) consult-cand))
        (should (eq (get-text-property 0 'consult-location (car returned))
                    'marker))))))

(ert-deftest nucleo-completion-initial-candidates-binds-regexp-list-test ()
  (let (seen-regexp-list)
    (cl-labels ((table (_string _pred action)
                  (when (eq action t)
                    (setq seen-regexp-list completion-regexp-list)
                    '("foo" "bar"))))
      (should (equal (nucleo-completion--initial-completion-candidates
                      "" "fo" #'table nil '("f.*o"))
                     '("foo" "bar")))
      (should (equal seen-regexp-list '("f.*o"))))))

(ert-deftest nucleo-completion-filter-test ()
  (let ((completion-ignore-case nil))
    (should (equal (nucleo-completion-tests--plain
                    (nucleo-completion-all-completions
                     "fb" '("foobar" "fxxx" "foo-baz" "" "fb")))
                   '("fb" "foo-baz" "foobar")))))

(ert-deftest nucleo-completion-fallback-keeps-input-order-without-sort-test ()
  (let ((completion-ignore-case nil)
        (nucleo-completion-sort-ties-by-history nil)
        (nucleo-completion-sort-ties-by-length nil)
        (nucleo-completion-sort-ties-alphabetically nil)
        (nucleo-completion-highlight-score-bands t)
        (nucleo-completion-max-highlighted-completions 10))
    (cl-letf (((symbol-function 'nucleo-completion--module-ready-p)
               (lambda () nil))
              ((symbol-function 'nucleo-completion-candidates)
               (lambda (&rest _)
                 (error "Fallback must not call the Rust candidate API"))))
      (let ((all (nucleo-completion-all-completions
                  "fb" '("foo-baz" "fb" "foobar" "bar"))))
        (should (equal (nucleo-completion-tests--plain all)
                       '("foo-baz" "fb" "foobar")))
        (dolist (candidate all)
          (let ((faces (ensure-list (get-text-property 0 'face candidate))))
            (should-not (memq 'nucleo-completion-high-score-face faces))
            (should-not (memq 'nucleo-completion-low-score-face faces))))))))

(ert-deftest nucleo-completion-fallback-sort-ties-by-length-test ()
  (let ((completion-ignore-case nil)
        (nucleo-completion-sort-ties-by-history nil)
        (nucleo-completion-sort-ties-by-length t)
        (nucleo-completion-sort-ties-alphabetically nil))
    (cl-letf (((symbol-function 'nucleo-completion--module-ready-p)
               (lambda () nil))
              ((symbol-function 'nucleo-completion-candidates)
               (lambda (&rest _)
                 (error "Fallback must not call the Rust candidate API"))))
      (should (equal (nucleo-completion-tests--plain
                      (nucleo-completion-all-completions
                       "fb" '("foo-baz" "fb" "foobar" "bar")))
                     '("fb" "foobar" "foo-baz"))))))

(ert-deftest nucleo-completion-fallback-sort-ties-alphabetically-test ()
  (let ((completion-ignore-case nil)
        (nucleo-completion-sort-ties-by-history nil)
        (nucleo-completion-sort-ties-by-length nil)
        (nucleo-completion-sort-ties-alphabetically t))
    (cl-letf (((symbol-function 'nucleo-completion--module-ready-p)
               (lambda () nil))
              ((symbol-function 'nucleo-completion-candidates)
               (lambda (&rest _)
                 (error "Fallback must not call the Rust candidate API"))))
      (should (equal (nucleo-completion-tests--plain
                      (nucleo-completion-all-completions
                       "a" '("beta" "alpha" "aardvark")))
                     '("aardvark" "alpha" "beta"))))))

(ert-deftest nucleo-completion-fallback-sort-skips-singleton-work-test ()
  (let ((nucleo-completion-sort-ties-by-history t)
        (nucleo-completion-sort-ties-by-length t)
        (nucleo-completion-sort-ties-alphabetically t))
    (cl-letf (((symbol-function 'nucleo-completion--history-rank-table)
               (lambda (_prefix)
                 (error "singleton fallback results do not need history")))
              ((symbol-function 'nucleo-completion--fallback-alphabetical-key)
               (lambda (_candidate)
                 (error "singleton fallback results do not need sort keys"))))
      (should (equal (nucleo-completion--fallback-sort '("foo") "")
                     '("foo")))
      (should-not (nucleo-completion--fallback-sort nil "")))))

(ert-deftest nucleo-completion-fallback-sort-ties-history-before-length-test ()
  (let ((completion-ignore-case nil)
        (nucleo-completion-sort-ties-by-history t)
        (nucleo-completion-sort-ties-by-length t)
        (nucleo-completion-sort-ties-alphabetically t)
        (minibuffer-history-variable 'nucleo-completion-tests-history)
        (nucleo-completion-tests-history '("alphabet" "alpha")))
    (cl-letf (((symbol-function 'nucleo-completion--module-ready-p)
               (lambda () nil))
              ((symbol-function 'minibufferp)
               (lambda (&rest _) t))
              ((symbol-function 'nucleo-completion-candidates)
               (lambda (&rest _)
                 (error "Fallback must not call the Rust candidate API"))))
      (should (equal (nucleo-completion-tests--plain
                      (nucleo-completion-all-completions
                       "alp" '("alpaca" "alpha" "alphabet")))
                     '("alphabet" "alpha" "alpaca"))))))

(ert-deftest nucleo-completion-fallback-sort-ties-by-corfu-history-test ()
  (let ((completion-ignore-case nil)
        (nucleo-completion-sort-ties-by-history t)
        (nucleo-completion-sort-ties-by-length nil)
        (nucleo-completion-sort-ties-alphabetically nil)
        (corfu-history-mode t)
        (corfu-history '("alpha" "beta")))
    (cl-letf (((symbol-function 'nucleo-completion--module-ready-p)
               (lambda () nil))
              ((symbol-function 'minibufferp)
               (lambda (&rest _) nil))
              ((symbol-function 'nucleo-completion-candidates)
               (lambda (&rest _)
                 (error "Fallback must not call the Rust candidate API"))))
      (should (equal (nucleo-completion-tests--plain
                      (nucleo-completion-all-completions
                       "a" '("beta" "alpha" "aardvark")))
                     '("alpha" "beta" "aardvark"))))))

(ert-deftest nucleo-completion-native-module-smoke-test ()
  (unless (nucleo-completion--module-ready-p)
    (ert-skip "Rust module is not available"))
  (let ((completion-ignore-case nil))
    (should (equal (nucleo-completion--module-filter
                    "fb" '("foobar" "fxxx" "foo-baz" "" "fb") nil)
                   '("fb" "foo-baz" "foobar")))))

(ert-deftest nucleo-completion-native-module-interrupts-candidate-list-build-test ()
  (unless (nucleo-completion--module-ready-p)
    (ert-skip "Rust module is not available"))
  (let ((calls 0)
        (candidates (nucleo-completion-tests--numbered-a-candidates 600)))
    (cl-letf (((symbol-function 'input-pending-p)
               (lambda (&optional _check-timers)
                 (setq calls (1+ calls))
                 (>= calls 6))))
      (let ((bundle (nucleo-completion-candidates
                     "a" candidates nil nil nil 0 nil)))
        (should (nucleo-completion--bundle-interrupted-p bundle))
        (should (= calls 6))))))

(ert-deftest nucleo-completion-native-module-interrupts-top-info-build-test ()
  (unless (nucleo-completion--module-ready-p)
    (ert-skip "Rust module is not available"))
  (let ((calls 0)
        (candidates (nucleo-completion-tests--numbered-a-candidates 300)))
    (cl-letf (((symbol-function 'input-pending-p)
               (lambda (&optional _check-timers)
                 (setq calls (1+ calls))
                 (>= calls 7))))
      (let ((bundle (nucleo-completion-candidates
                     "a" candidates nil nil nil 300 nil)))
        (should (nucleo-completion--bundle-interrupted-p bundle))
        (should (= calls 7))))))

(ert-deftest nucleo-completion-native-module-interrupts-full-score-build-test ()
  (unless (nucleo-completion--module-ready-p)
    (ert-skip "Rust module is not available"))
  (let ((calls 0)
        (candidates (nucleo-completion-tests--numbered-a-candidates 300)))
    (cl-letf (((symbol-function 'input-pending-p)
               (lambda (&optional _check-timers)
                 (setq calls (1+ calls))
                 (>= calls 8))))
      (let ((bundle (nucleo-completion-candidates
                     "a" candidates nil nil nil 0 t)))
        (should (nucleo-completion--bundle-interrupted-p bundle))
        (should (= calls 8))))))

(ert-deftest nucleo-completion-native-module-score-property-test ()
  (unless (nucleo-completion--module-ready-p)
    (ert-skip "Rust module is not available"))
  (let* ((bundle (nucleo-completion--module-results
                  "fb" '("foobar" "foo-baz" "fb") nil 0 t))
         (candidate (car (nucleo-completion--bundle-candidates bundle)))
         (score (car (nucleo-completion--bundle-full-scores bundle))))
    (should (integerp score))
    (should (equal (nucleo-completion--candidate-score candidate) score))
    (should (equal (nucleo-completion--candidate-score
                    (copy-sequence candidate))
                   score))))

(ert-deftest nucleo-completion-ensure-score-properties-fills-partial-test ()
  (let* ((scored (nucleo-completion--propertize-score "foo" 100))
         (plain "bar")
         (bundle (list (list scored plain) nil '(100 50)))
         (returned (nucleo-completion--bundle-candidates
                    (nucleo-completion--ensure-score-properties bundle))))
    (should (eq (car returned) scored))
    (should (equal (nucleo-completion--candidate-score (car returned))
                   100))
    (should (equal (nucleo-completion--candidate-score (cadr returned))
                   50))))

(ert-deftest nucleo-completion-ensure-score-properties-scans-once-test ()
  (let* ((scored (nucleo-completion--propertize-score "foo" 100))
         (plain "bar")
         (bundle (list (list scored plain) nil '(100 50)))
         (candidate-score (symbol-function 'nucleo-completion--candidate-score))
         (calls 0))
    (cl-letf (((symbol-function 'nucleo-completion--candidate-score)
               (lambda (candidate)
                 (setq calls (1+ calls))
                 (funcall candidate-score candidate))))
      (let ((returned (nucleo-completion--bundle-candidates
                       (nucleo-completion--ensure-score-properties bundle))))
        (should (eq (car returned) scored))
        (should (equal (nucleo-completion--candidate-score (cadr returned))
                       50))
        (should (= calls 3))))))

(ert-deftest nucleo-completion-ensure-score-properties-keeps-extra-candidates-test ()
  (let* ((bundle (list '("foo" "bar") nil '(100)))
         (returned (nucleo-completion--bundle-candidates
                    (nucleo-completion--ensure-score-properties bundle))))
    (should (equal (nucleo-completion-tests--plain returned)
                   '("foo" "bar")))
    (should (equal (nucleo-completion--candidate-score (car returned))
                   100))
    (should-not (nucleo-completion--candidate-score (cadr returned)))))

(ert-deftest nucleo-completion-module-completion-results-ensures-scores-once-test ()
  (let ((completion-ignore-case nil)
        (calls 0)
        (original-ensure
         (symbol-function 'nucleo-completion--ensure-score-properties)))
    (cl-letf (((symbol-function 'nucleo-completion-candidates)
               (lambda (_needle _candidates _ignore-case _by-length
                                _alphabetically _limit
                                &optional return-all-scores)
                 (list '("foo" "fob")
                       nil
                       (when return-all-scores '(100 50)))))
              ((symbol-function 'nucleo-completion--ensure-score-properties)
               (lambda (bundle)
                 (setq calls (1+ calls))
                 (funcall original-ensure bundle))))
      (pcase-let ((`(,all ,_bundle ,_top-info ,_full-scores)
                   (nucleo-completion--legacy-module-completion-results
                    "" "fo" '("foo" "fob") nil 0 t)))
        (should (equal (nucleo-completion-tests--plain all)
                       '("foo" "fob")))
        (should (= calls 1))))))

(ert-deftest nucleo-completion-native-module-version-test ()
  (unless (nucleo-completion--module-ready-p)
    (ert-skip "Rust module is not available"))
  (should (equal (nucleo-completion-module-version)
                 nucleo-completion-required-module-version)))

(ert-deftest nucleo-completion-native-module-history-sort-test ()
  (unless (nucleo-completion--module-supports-history-p)
    (ert-skip "Rust module with history sorting is not available"))
  (let ((bundle (nucleo-completion--module-results
                 "a" '("ab" "aa" "ba") nil 0 nil '(1 0 nil))))
    (should (equal (nucleo-completion--bundle-candidates bundle)
                   '("aa" "ab" "ba")))))

(ert-deftest nucleo-completion-native-module-short-history-ranks-test ()
  (unless (nucleo-completion--module-supports-history-p)
    (ert-skip "Rust module with history sorting is not available"))
  (let ((bundle (nucleo-completion-candidates-with-history
                 "a" '("aa" "ab" "ac") nil nil nil '(1 0) 0 t)))
    (should (equal (nucleo-completion--bundle-candidates bundle)
                   '("ab" "aa" "ac")))))

(ert-deftest nucleo-completion-requires-bundle-module-api-test ()
  (cl-letf (((symbol-function 'nucleo-completion-candidates)
             (lambda (_needle _candidates _ignore-case _by-length
                              _alphabetically _limit)
               nil)))
    (should-error
     (nucleo-completion--call-module "fb" '("fb") nil 0 nil)
     :type 'wrong-number-of-arguments)))

(ert-deftest nucleo-completion-module-ready-checks-version-test ()
  (let ((nucleo-completion-required-module-version "9.8.7")
        (nucleo-completion--module-version-warning-shown nil)
        warnings)
    (cl-letf (((symbol-function 'nucleo-completion-candidates)
               (lambda (&rest _) nil))
              ((symbol-function 'nucleo-completion-module-version)
               (lambda () "9.8.6"))
              ((symbol-function 'display-warning)
               (lambda (type message &optional level buffer-name)
                 (push (list type message level buffer-name) warnings))))
      (should-not (nucleo-completion--module-ready-p))
      (should (= (length warnings) 1))
      (should (string-match-p "expects 9.8.7" (cadar warnings)))
      (should-not (nucleo-completion--module-ready-p))
      (should (= (length warnings) 1))))
  (let ((nucleo-completion-required-module-version "9.8.7")
        (nucleo-completion--module-version-warning-shown nil))
    (cl-letf (((symbol-function 'nucleo-completion-candidates)
               (lambda (&rest _) nil))
              ((symbol-function 'nucleo-completion-module-version)
               (lambda () "9.8.7"))
              ((symbol-function 'display-warning)
               (lambda (&rest _)
                 (error "Compatible module must not warn"))))
      (should (nucleo-completion--module-ready-p)))))

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

(ert-deftest nucleo-completion-long-candidates-use-module-scoring-test ()
  (let ((completion-ignore-case nil)
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
                 (should (equal candidates (list long-match "fb" long-miss)))
                 (nucleo-completion-tests--bundle `(("fb" 100 (0 1))
                                                    (,long-match 10 (0 4)))
                                                  return-all-scores))))
      (let ((all (nucleo-completion-all-completions
                  "fb" (list long-match "fb" long-miss))))
        (should (equal (nucleo-completion-tests--plain all)
                       '("fb" "foo-baz")))
        (should
         (memq 'nucleo-completion-low-score-face
               (ensure-list (get-text-property 0 'face (cadr all)))))))))

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

(ert-deftest nucleo-completion-module-interrupt-reuses-last-result-test ()
  (let* ((completion-ignore-case nil)
         (table '("foo" "bar" "fob"))
         (nucleo-completion--current-prefix "")
         (nucleo-completion--current-pattern "f")
         (nucleo-completion--current-context
          (nucleo-completion-tests--context table))
         (nucleo-completion--current-result '("foo" "fb")))
    (cl-letf (((symbol-function 'nucleo-completion--module-ready-p)
               (lambda () t))
              ((symbol-function 'nucleo-completion-candidates)
               (lambda (&rest _)
                 (list nucleo-completion--interrupted-sentinel nil nil))))
      (should (equal (nucleo-completion-all-completions
                      "fo" table nil nil)
                     '("foo")))
      (should (equal nucleo-completion--current-result '("foo" "fb"))))))

(ert-deftest nucleo-completion-module-interrupt-skips-shorter-pattern-reuse-test ()
  (let* ((completion-ignore-case nil)
         (table '("foo" "fob" "bar"))
         (nucleo-completion--current-prefix "")
         (nucleo-completion--current-pattern "foo")
         (nucleo-completion--current-context
          (nucleo-completion-tests--context table))
         (nucleo-completion--current-result '("foo")))
    (cl-letf (((symbol-function 'nucleo-completion--module-ready-p)
               (lambda () t))
              ((symbol-function 'nucleo-completion-candidates)
               (lambda (&rest _)
                 (list nucleo-completion--interrupted-sentinel nil nil))))
      (should-not (nucleo-completion-all-completions
                   "fo" table nil nil))
      (should (equal nucleo-completion--current-result '("foo"))))))

(ert-deftest nucleo-completion-current-pattern-refinement-skips-regexp-functions-test ()
  (let ((completion-ignore-case nil)
        (nucleo-completion-regexp-functions (list #'identity))
        (nucleo-completion--current-pattern "f"))
    (should (nucleo-completion--current-pattern-refinement-p "f"))
    (should-not (nucleo-completion--current-pattern-refinement-p "fo"))))

(ert-deftest nucleo-completion-all-completions-while-no-input-reuses-last-result-test ()
  (let* ((completion-ignore-case nil)
         (table '("foo" "bar"))
         (nucleo-completion--current-prefix "")
         (nucleo-completion--current-pattern "f")
         (nucleo-completion--current-base-size nil)
         (nucleo-completion--current-context
          (nucleo-completion-tests--context table))
         (nucleo-completion--current-result '("foo"))
         (unread-command-events (list ?x)))
    (should (equal (nucleo-completion-all-completions
                    "fo" table nil nil)
                   '("foo")))))

(ert-deftest nucleo-completion-all-completions-interrupt-refreshes-lazy-highlight-test ()
  (let ((completion-ignore-case nil)
        (completion-lazy-hilit t)
        (table '("foo" "fob" "bar")))
    (cl-letf (((symbol-function 'nucleo-completion--module-ready-p)
               (lambda () nil)))
      (should (equal (nucleo-completion-tests--plain
                      (nucleo-completion-all-completions
                       "f" table nil nil))
                     '("foo" "fob")))
      (let ((old-highlight completion-lazy-hilit-fn)
            (unread-command-events (list ?x)))
        (should (equal (nucleo-completion-all-completions
                        "fb" table nil nil)
                       '("fob")))
        (should-not (eq completion-lazy-hilit-fn old-highlight))
        (let ((highlighted (funcall completion-lazy-hilit-fn "fob")))
          (should (memq 'completions-common-part
                        (ensure-list (get-text-property
                                      0 'face highlighted))))
          (should-not (get-text-property 1 'face highlighted))
          (should (memq 'completions-common-part
                        (ensure-list (get-text-property
                                      2 'face highlighted)))))))))

(ert-deftest nucleo-completion-all-completions-interrupt-skips-table-mismatch-test ()
  (let* ((completion-ignore-case nil)
         (old-table '("foo"))
         (new-table '("bar"))
         (nucleo-completion--current-prefix "")
         (nucleo-completion--current-pattern "f")
         (nucleo-completion--current-base-size nil)
         (nucleo-completion--current-context
          (nucleo-completion-tests--context old-table))
         (nucleo-completion--current-result '("foo"))
         (unread-command-events (list ?x)))
    (should-not (nucleo-completion-all-completions
                 "fo" new-table nil nil))))

(ert-deftest nucleo-completion-all-completions-interrupt-skips-regexp-list-mismatch-test ()
  (let* ((completion-ignore-case nil)
         (table '("foo"))
         (completion-regexp-list '("bar"))
         (nucleo-completion--current-prefix "")
         (nucleo-completion--current-pattern "f")
         (nucleo-completion--current-base-size nil)
         (nucleo-completion--current-context
          (nucleo-completion-tests--context table nil nil))
         (nucleo-completion--current-result '("foo"))
         (unread-command-events (list ?x)))
    (should-not (nucleo-completion-all-completions
                 "fo" table nil nil))))

(ert-deftest nucleo-completion-all-completions-interrupt-skips-prefix-mismatch-test ()
  (let ((completion-ignore-case nil)
        (nucleo-completion--current-prefix "old/")
        (nucleo-completion--current-pattern "fo")
        (nucleo-completion--current-base-size nil)
        (nucleo-completion--current-result '("stale"))
        (unread-command-events (list ?x)))
    (cl-labels ((table (_string _pred action)
                  (cond
                   ((eq (car-safe action) 'boundaries)
                    (cons 'boundaries (cons (length "new/") 0)))
                   ((eq action t) '("foo"))
                   ((eq action 'metadata) nil)
                   ((eq action nil) nil))))
      (should-not (nucleo-completion-all-completions
                   "new/fo" #'table nil (length "new/fo"))))))

(ert-deftest nucleo-completion-all-completions-module-interrupt-filters-last-result-test ()
  (let* ((completion-ignore-case nil)
         (table '("zoo" "bar"))
         (nucleo-completion--current-prefix "")
         (nucleo-completion--current-pattern "z")
         (nucleo-completion--current-base-size nil)
         (nucleo-completion--current-context
          (nucleo-completion-tests--context table))
         (nucleo-completion--current-result '("zoo")))
    (cl-letf (((symbol-function 'nucleo-completion--module-ready-p)
               (lambda () t))
              ((symbol-function 'nucleo-completion-candidates)
               (lambda (&rest _)
                 (list nucleo-completion--interrupted-sentinel nil nil))))
      (should-not (nucleo-completion-all-completions
                   "zz" table nil nil))
      (should (equal nucleo-completion--current-result '("zoo"))))))

(ert-deftest nucleo-completion-all-completions-interrupt-preserves-ranked-order-test ()
  (let* ((completion-ignore-case nil)
         (nucleo-completion-sort-ties-by-length t)
         (nucleo-completion-sort-ties-by-history nil)
         (nucleo-completion-sort-ties-alphabetically nil)
         (table '("fb" "foobar"))
         (nucleo-completion--current-prefix "")
         (nucleo-completion--current-pattern "f")
         (nucleo-completion--current-base-size nil)
         (nucleo-completion--current-context
          (nucleo-completion-tests--context table))
         (nucleo-completion--current-result '("foobar" "fb")))
    (cl-letf (((symbol-function 'nucleo-completion--module-ready-p)
               (lambda () t))
              ((symbol-function 'nucleo-completion-candidates)
               (lambda (&rest _)
                 (list nucleo-completion--interrupted-sentinel nil nil))))
      (should (equal (nucleo-completion-all-completions
                      "fb" table nil nil)
                     '("foobar" "fb"))))))

(ert-deftest nucleo-completion-try-module-interrupt-reuses-last-result-test ()
  (let* ((completion-ignore-case nil)
         (table '("foo bar" "bar foo"))
         (nucleo-completion--current-prefix "")
         (nucleo-completion--current-pattern "fo b")
         (nucleo-completion--current-base-size nil)
         (nucleo-completion--current-context
          (nucleo-completion-tests--context table))
         (nucleo-completion--current-result '("foo bar")))
    (cl-letf (((symbol-function 'nucleo-completion--module-ready-p)
               (lambda () t))
              ((symbol-function 'nucleo-completion-candidates)
               (lambda (&rest _)
                 (list nucleo-completion--interrupted-sentinel nil nil))))
      (should (equal (nucleo-completion-try-completion
                      "fo ba" table nil nil)
                     '("foo bar" . 7)))
      (should (equal nucleo-completion--current-result '("foo bar"))))))

(ert-deftest nucleo-completion-try-interrupt-skips-shorter-pattern-reuse-test ()
  (let* ((completion-ignore-case nil)
         (table '("foo bar" "fob bang" "bar"))
         (nucleo-completion--current-prefix "")
         (nucleo-completion--current-pattern "foo bar")
         (nucleo-completion--current-base-size nil)
         (nucleo-completion--current-context
          (nucleo-completion-tests--context table))
         (nucleo-completion--current-result '("foo bar")))
    (cl-letf (((symbol-function 'nucleo-completion--module-ready-p)
               (lambda () t))
              ((symbol-function 'nucleo-completion-candidates)
               (lambda (&rest _)
                 (list nucleo-completion--interrupted-sentinel nil nil))))
      (should-not (nucleo-completion-try-completion
                   "fo ba" table nil nil))
      (should (equal nucleo-completion--current-result '("foo bar"))))))

(ert-deftest nucleo-completion-try-while-no-input-reuses-last-result-test ()
  (let* ((completion-ignore-case nil)
         (table '("foo" "bar"))
         (nucleo-completion--current-prefix "")
         (nucleo-completion--current-pattern "f")
         (nucleo-completion--current-base-size nil)
         (nucleo-completion--current-context
          (nucleo-completion-tests--context table))
         (nucleo-completion--current-result '("foo"))
         (unread-command-events (list ?x)))
    (should (equal (nucleo-completion-try-completion
                    "fo" table nil nil)
                   '("foo" . 3)))))

(ert-deftest nucleo-completion-try-interrupt-skips-table-mismatch-test ()
  (let* ((completion-ignore-case nil)
         (old-table '("foo"))
         (new-table '("bar"))
         (nucleo-completion--current-prefix "")
         (nucleo-completion--current-pattern "f")
         (nucleo-completion--current-base-size nil)
         (nucleo-completion--current-context
          (nucleo-completion-tests--context old-table))
         (nucleo-completion--current-result '("foo"))
         (unread-command-events (list ?x)))
    (should-not (nucleo-completion-try-completion
                 "fo" new-table nil nil))))

(ert-deftest nucleo-completion-try-module-interrupt-filters-last-result-test ()
  (let* ((completion-ignore-case nil)
         (table '("zoo" "bar"))
         (nucleo-completion--current-prefix "")
         (nucleo-completion--current-pattern "z")
         (nucleo-completion--current-base-size nil)
         (nucleo-completion--current-context
          (nucleo-completion-tests--context table))
         (nucleo-completion--current-result '("zoo")))
    (cl-letf (((symbol-function 'nucleo-completion--module-ready-p)
               (lambda () t))
              ((symbol-function 'nucleo-completion-candidates)
               (lambda (&rest _)
                 (list nucleo-completion--interrupted-sentinel nil nil))))
      (should-not (nucleo-completion-try-completion
                   "zz" table nil nil))
      (should (equal nucleo-completion--current-result '("zoo"))))))

(ert-deftest nucleo-completion-try-interrupt-preserves-ranked-order-test ()
  (let* ((completion-ignore-case nil)
         (nucleo-completion-sort-ties-by-length t)
         (nucleo-completion-sort-ties-by-history nil)
         (nucleo-completion-sort-ties-alphabetically nil)
         (table '("fb" "foobar"))
         (nucleo-completion--current-prefix "")
         (nucleo-completion--current-pattern "f")
         (nucleo-completion--current-base-size nil)
         (nucleo-completion--current-context
          (nucleo-completion-tests--context table))
         (nucleo-completion--current-result '("foobar" "fb"))
         seen-candidates)
    (cl-letf (((symbol-function 'nucleo-completion--try-result)
               (lambda (_string _point _prefix _pattern _suffix candidates)
                 (setq seen-candidates candidates)
                 t)))
      (should (eq (nucleo-completion--try-completion-interrupted-result
                   "fb" table nil nil)
                  t))
      (should (equal seen-candidates '("foobar" "fb"))))))

(ert-deftest nucleo-completion-current-result-stays-unhighlighted-test ()
  "Interrupt reuse stores filtered candidates before display highlighting."
  (let ((completion-ignore-case nil)
        (completion-lazy-hilit nil)
        (nucleo-completion-max-highlighted-completions 10))
    (cl-letf (((symbol-function 'nucleo-completion--module-ready-p)
               (lambda () t))
              ((symbol-function 'nucleo-completion-candidates)
               (lambda (_needle _candidates _ignore-case _by-length
                                _alphabetically _limit
                                &optional return-all-scores)
                 (nucleo-completion-tests--bundle
                  '(("foo" 100 (0 1)) ("fob" 90 (0 1)))
                  return-all-scores))))
      (let ((all (nucleo-completion-all-completions
                  "fo" '("foo" "fob" "bar") nil nil)))
        (should (get-text-property 0 'face (car all)))
        (should (equal nucleo-completion--current-pattern "fo"))
        (should (equal nucleo-completion--current-result
                       '("foo" "fob")))
        (should-not (get-text-property
                     0 'face (car nucleo-completion--current-result)))))))

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

(ert-deftest nucleo-completion-empty-input-clears-state-test ()
  (let ((completion-lazy-hilit t)
        (completion-lazy-hilit-fn (lambda (_candidate) "stale"))
        (nucleo-completion--filtering-p t)
        (nucleo-completion--current-pattern "stale")
        (nucleo-completion--current-context 'stale)
        (nucleo-completion--current-result '("stale")))
    (should (equal (nucleo-completion-all-completions
                    "" '("foo" "bar") nil 0)
                   '("foo" "bar")))
    (should-not nucleo-completion--filtering-p)
    (should-not nucleo-completion--current-pattern)
    (should-not nucleo-completion--current-context)
    (should-not nucleo-completion--current-result)
    (should-not completion-lazy-hilit-fn)))

(ert-deftest nucleo-completion-empty-input-clears-state-before-table-test ()
  (let ((completion-lazy-hilit t)
        (completion-lazy-hilit-fn (lambda (_candidate) "stale"))
        (nucleo-completion--filtering-p t)
        (nucleo-completion--current-pattern "stale")
        (nucleo-completion--current-context 'stale)
        (nucleo-completion--current-result '("stale")))
    (cl-letf (((symbol-function
                'nucleo-completion--initial-completion-candidates)
               (lambda (_prefix _needle _table _pred _regexp-list)
                 (should-not nucleo-completion--filtering-p)
                 (should-not nucleo-completion--current-pattern)
                 (should-not nucleo-completion--current-context)
                 (should-not nucleo-completion--current-result)
                 (should-not completion-lazy-hilit-fn)
                 '("foo"))))
      (should (equal (nucleo-completion-all-completions
                      "" '("foo") nil 0)
                     '("foo"))))))

(ert-deftest nucleo-completion-empty-input-skips-pass-cache-test ()
  (let ((nucleo-completion-regexp-functions nil)
        (make-hash-table-calls 0)
        (original-make-hash-table (symbol-function 'make-hash-table)))
    (cl-letf (((symbol-function 'make-hash-table)
               (lambda (&rest args)
                 (setq make-hash-table-calls (1+ make-hash-table-calls))
                 (apply original-make-hash-table args))))
      (should (equal (nucleo-completion-all-completions
                      "" '("foo" "bar") nil 0)
                     '("foo" "bar")))
      (should (= make-hash-table-calls 0)))))

(ert-deftest nucleo-completion-try-empty-input-clears-state-test ()
  (let ((completion-lazy-hilit-fn (lambda (_candidate) "stale"))
        (nucleo-completion--filtering-p t)
        (nucleo-completion--current-pattern "stale")
        (nucleo-completion--current-context 'stale)
        (nucleo-completion--current-result '("stale")))
    (should (equal (nucleo-completion-try-completion
                    "" '("foo" "bar") nil 0)
                   '("" . 0)))
    (should-not nucleo-completion--filtering-p)
    (should-not nucleo-completion--current-pattern)
    (should-not nucleo-completion--current-context)
    (should-not nucleo-completion--current-result)
    (should-not completion-lazy-hilit-fn)))

(ert-deftest nucleo-completion-try-empty-input-skips-pass-cache-test ()
  (let ((nucleo-completion-regexp-functions nil)
        (make-hash-table-calls 0)
        (original-make-hash-table (symbol-function 'make-hash-table)))
    (cl-letf (((symbol-function 'make-hash-table)
               (lambda (&rest args)
                 (setq make-hash-table-calls (1+ make-hash-table-calls))
                 (apply original-make-hash-table args))))
      (should (equal (nucleo-completion-try-completion
                      "" '("foo" "bar") nil 0)
                     '("" . 0)))
      (should (= make-hash-table-calls 0)))))

(ert-deftest nucleo-completion-try-flex-success-clears-reusable-state-test ()
  (let ((completion-lazy-hilit-fn (lambda (_candidate) "stale"))
        (nucleo-completion--filtering-p t)
        (nucleo-completion--current-prefix "old")
        (nucleo-completion--current-pattern "stale")
        (nucleo-completion--current-base-size 0)
        (nucleo-completion--current-context 'stale)
        (nucleo-completion--current-result '("stale")))
    (should (equal (nucleo-completion-try-completion
                    "fo" '("foo" "fob" "bar") nil 2)
                   '("fo" . 2)))
    (should nucleo-completion--filtering-p)
    (should (equal nucleo-completion--current-prefix ""))
    (should-not nucleo-completion--current-pattern)
    (should-not nucleo-completion--current-base-size)
    (should-not nucleo-completion--current-context)
    (should-not nucleo-completion--current-result)
    (should-not completion-lazy-hilit-fn)))

(ert-deftest nucleo-completion-table-base-size-test ()
  (let ((completion-ignore-case nil))
    (cl-labels ((table (_string _pred action)
                  (cond
                   ((eq action t) '("foo" "fob" "bar" . 0))
                   ((eq action 'metadata) nil)
                   ((eq action nil) "foo"))))
      (cl-letf (((symbol-function 'nucleo-completion--module-ready-p)
                 (lambda () t))
                ((symbol-function 'nucleo-completion-candidates)
                 (lambda (_needle candidates _ignore-case _by-length
                                  _alphabetically _limit
                                  &optional return-all-scores)
                   (should (equal candidates '("foo" "fob" "bar")))
                   (nucleo-completion-tests--bundle
                    '(("foo" 100 nil) ("fob" 90 nil))
                    return-all-scores))))
        (pcase-let ((`(,candidates . ,base-size)
                     (nucleo-completion--split-base-size
                      (nucleo-completion-all-completions
                       "fo" #'table nil 2))))
          (should (equal (nucleo-completion-tests--plain candidates)
                         '("foo" "fob")))
          (should (= base-size 0)))))))

(ert-deftest nucleo-completion-list-table-skips-base-size-scan-test ()
  "Plain list tables do not need a tail scan for base-size markers."
  (let ((completion-ignore-case nil))
    (cl-letf (((symbol-function 'nucleo-completion--module-ready-p)
               (lambda () nil))
              ((symbol-function 'nucleo-completion--split-base-size)
               (lambda (_candidates)
                 (error "base-size scan should be skipped"))))
      (should (equal (nucleo-completion-tests--plain
                      (nucleo-completion-all-completions
                       "fo" '("foo" "fob" "bar") nil nil))
                     '("foo" "fob"))))))

(ert-deftest nucleo-completion-list-table-skips-unused-regexp-list-test ()
  (let ((completion-ignore-case nil))
    (cl-letf (((symbol-function 'nucleo-completion--module-ready-p)
               (lambda () nil))
              ((symbol-function 'nucleo-completion--completion-regexp-list)
               (lambda (&rest _)
                 (error "Plain list fast path uses Nucleo's own filter"))))
      (should (equal (nucleo-completion-tests--plain
                      (nucleo-completion-all-completions
                       "fo" '("foo" "fob" "bar") nil nil))
                     '("foo" "fob"))))))

(ert-deftest nucleo-completion-list-table-respects-external-regexp-list-test ()
  (let ((completion-ignore-case nil)
        (completion-regexp-list '("bar")))
    (cl-letf (((symbol-function 'nucleo-completion--module-ready-p)
               (lambda () nil)))
      (should-not (nucleo-completion-all-completions
                   "fo" '("foo" "fob") nil nil)))))

(ert-deftest nucleo-completion-interrupt-preserves-base-size-test ()
  (let* ((completion-ignore-case nil)
         (table '("foo" "fob"))
         (nucleo-completion--current-prefix "")
         (nucleo-completion--current-pattern "f")
         (nucleo-completion--current-base-size 0)
         (nucleo-completion--current-context
          (nucleo-completion-tests--context table))
         (nucleo-completion--current-result '("foo")))
    (cl-letf (((symbol-function 'nucleo-completion--module-ready-p)
               (lambda () t))
              ((symbol-function 'nucleo-completion-candidates)
               (lambda (&rest _)
                 (list nucleo-completion--interrupted-sentinel nil nil))))
      (pcase-let ((`(,candidates . ,base-size)
                   (nucleo-completion--split-base-size
                    (nucleo-completion-all-completions
                     "fo" table nil nil))))
        (should (equal candidates '("foo")))
        (should (= base-size 0))))))

(ert-deftest nucleo-completion-try-completion-records-base-size-test ()
  (let ((completion-ignore-case nil)
        (nucleo-completion--current-prefix "")
        (nucleo-completion--current-base-size nil)
        (nucleo-completion--current-result nil))
    (cl-labels ((table (_string _pred action)
                  (cond
                   ((eq action t) '("foo bar" "foo baz" . 0))
                   ((eq action 'metadata) nil)
                   ((eq action nil) nil))))
      (cl-letf (((symbol-function 'nucleo-completion--module-ready-p)
                 (lambda () nil)))
        (should (nucleo-completion-try-completion "fo ba" #'table nil 5)))
      (cl-letf (((symbol-function 'nucleo-completion--module-ready-p)
                 (lambda () t))
                ((symbol-function 'nucleo-completion-candidates)
                 (lambda (&rest _)
                   (list nucleo-completion--interrupted-sentinel nil nil))))
        (pcase-let ((`(,candidates . ,base-size)
                     (nucleo-completion--split-base-size
                      (nucleo-completion-all-completions
                       "fo ba" #'table nil 5))))
          (should (equal candidates '("foo bar" "foo baz")))
          (should (= base-size 0)))))))

(ert-deftest nucleo-completion-base-size-combines-with-prefix-test ()
  (should (equal (nucleo-completion--with-base-size "ab" (list "foo") 1)
                 '("foo" . 3))))

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

(ert-deftest nucleo-completion-flex-nospace-test ()
  (let ((completion-flex-nospace t)
        (completion-ignore-case nil)
        (table '("foo/bar" "bar/foo" "foobar")))
    (should-not (nucleo-completion-all-completions
                 "foo bar" table nil 7))
    (should-not (nucleo-completion-try-completion
                 "foo bar" table nil 7))))

(ert-deftest nucleo-completion-flex-nospace-rejects-whitespace-test ()
  (let ((completion-flex-nospace t)
        (completion-ignore-case nil)
        (table '("foo/bar" "bar/foo" "foobar")))
    (cl-letf (((symbol-function 'nucleo-completion--module-ready-p)
               (lambda () nil)))
      (dolist (input (list (concat "foo" (string ?\t) "bar")
                           (concat "foo" (string ?\n) "bar")))
        (should-not (nucleo-completion-all-completions
                     input table nil (length input)))
        (should-not (nucleo-completion-try-completion
                     input table nil (length input)))))))

(ert-deftest nucleo-completion-flex-nospace-skips-pass-cache-test ()
  (let ((completion-flex-nospace t)
        (completion-ignore-case nil)
        (nucleo-completion-regexp-functions nil)
        (table '("foo/bar" "bar/foo" "foobar"))
        (make-hash-table-calls 0)
        (original-make-hash-table (symbol-function 'make-hash-table)))
    (cl-letf (((symbol-function 'make-hash-table)
               (lambda (&rest args)
                 (setq make-hash-table-calls (1+ make-hash-table-calls))
                 (apply original-make-hash-table args))))
      (should-not (nucleo-completion-all-completions
                   "foo bar" table nil 7))
      (should-not (nucleo-completion-try-completion
                   "foo bar" table nil 7))
      (should (= make-hash-table-calls 0)))))

(ert-deftest nucleo-completion-flex-nospace-checks-field-only-test ()
  (let* ((completion-flex-nospace t)
         (completion-ignore-case nil)
         (prefix "pre fix/")
         (input (concat prefix "fo")))
    (cl-labels ((table (_string _pred action)
                  (cond
                   ((eq (car-safe action) 'boundaries)
                    (cons 'boundaries (cons (length prefix) 0)))
                   ((eq action t) '("foo" "fob" "bar"))
                   ((eq action 'metadata) nil)
                   ((eq action nil) nil))))
      (pcase-let ((`(,candidates . ,base-size)
                   (nucleo-completion--split-base-size
                    (nucleo-completion-all-completions
                     input #'table nil (length input)))))
        (should (equal (nucleo-completion-tests--plain candidates)
                       '("foo" "fob")))
        (should (= base-size (length prefix)))))))

(ert-deftest nucleo-completion-no-result-clears-state-test ()
  (let ((completion-ignore-case nil)
        (completion-lazy-hilit t)
        (completion-lazy-hilit-fn (lambda (_candidate) "stale"))
        (nucleo-completion--filtering-p t))
    (cl-letf (((symbol-function 'nucleo-completion--module-ready-p)
               (lambda () nil)))
      (should-not (nucleo-completion-all-completions
                   "zz" '("foo" "bar") nil nil))
      (should-not nucleo-completion--filtering-p)
      (should-not completion-lazy-hilit-fn))))

(ert-deftest nucleo-completion-flex-nospace-clears-state-test ()
  (let ((completion-flex-nospace t)
        (completion-ignore-case nil)
        (completion-lazy-hilit t)
        (completion-lazy-hilit-fn (lambda (_candidate) "stale"))
        (nucleo-completion--filtering-p t))
    (should-not (nucleo-completion-all-completions
                 "foo bar" '("foo/bar" "bar/foo") nil 7))
    (should-not nucleo-completion--filtering-p)
    (should-not completion-lazy-hilit-fn)))

(ert-deftest nucleo-completion-all-completions-after-point-test ()
  (let* ((completion-styles '(nucleo))
         (table '("foo-bar" "foo-baz" "bar-foo"))
         (md (completion-metadata "fo-b" table nil))
         (all (completion-all-completions "fo-b" table nil 2 md)))
    (setcdr (last all) nil)
    (should (equal (nucleo-completion-tests--plain all)
                   '("foo-bar" "foo-baz")))))

(ert-deftest nucleo-completion-try-space-separated-test ()
  (let* ((completion-styles '(nucleo))
         (table '("foo/bar" "bar/foo" "foobar" "foo-qux" "bar-baz"))
         (md (completion-metadata "foo bar" table nil)))
    (should (equal (completion-try-completion "foo bar" table nil 7 md)
                   '("foo bar" . 7)))))

(ert-deftest nucleo-completion-try-after-point-filter-test ()
  (let* ((completion-styles '(nucleo))
         (table '("foo-qux-bar" "foo-qux-zap" "bar-foo-qux"))
         (md (completion-metadata "fo qux-b" table nil)))
    (cl-letf (((symbol-function 'nucleo-completion--module-ready-p)
               (lambda () nil)))
      (should (equal (completion-try-completion "fo qux-b" table nil 6 md)
                     '("foo-qux-bar" . 11))))))

(ert-deftest nucleo-completion-try-filter-reuses-field-state-test ()
  (let* ((completion-styles '(nucleo))
         (table '("foo/bar" "bar/foo" "foobar" "foo-qux" "bar-baz"))
         (md (completion-metadata "foo bar" table nil))
         (field-state (symbol-function 'nucleo-completion--field-state))
         (calls 0))
    (cl-letf (((symbol-function 'nucleo-completion--module-ready-p)
               (lambda () nil))
              ((symbol-function 'nucleo-completion--field-state)
               (lambda (&rest args)
                 (setq calls (1+ calls))
                 (apply field-state args))))
      (should (equal (completion-try-completion "foo bar" table nil 7 md)
                     '("foo bar" . 7)))
      (should (= calls 1)))))

(ert-deftest nucleo-completion-try-result-deduplicates-without-delete-dups-test ()
  (let ((candidate (copy-sequence "foo")))
    (add-text-properties 0 3 '(face bold) candidate)
    (cl-letf (((symbol-function 'delete-dups)
               (lambda (_list)
                 (error "try-result uses hash-based dedup"))))
      (should (equal (nucleo-completion--try-result
                      "f" 1 "" "f" "" (list candidate "foo" "fob"))
                     '("fo" . 2))))))

(ert-deftest nucleo-completion-try-result-keeps-plain-candidate-keys-test ()
  (cl-letf (((symbol-function 'substring-no-properties)
             (lambda (&rest _)
               (error "Plain try-completion candidates need no string copy"))))
    (should (equal (nucleo-completion--try-result
                    "f" 1 "" "f" "" '("foo" "fob"))
                   '("fo" . 2)))))

(ert-deftest nucleo-completion-try-result-skips-hash-for-pair-test ()
  (cl-letf (((symbol-function 'make-hash-table)
             (lambda (&rest _)
               (error "Two try-completion candidates need no hash table"))))
    (should (equal (nucleo-completion--try-result
                    "f" 1 "" "f" "" '("foo" "fob"))
                   '("fo" . 2)))))

(ert-deftest nucleo-completion-try-result-seen-table-skips-size-scan-test ()
  (let ((original-make-hash-table (symbol-function 'make-hash-table)))
    (cl-letf (((symbol-function 'make-hash-table)
               (lambda (&rest args)
                 (should-not (memq :size args))
                 (apply original-make-hash-table args))))
      (should (equal (nucleo-completion--try-result
                      "f" 1 "" "f" "" '("foo" "fob" "fox"))
                     '("fo" . 2))))))

(ert-deftest nucleo-completion-try-exact-after-point-test ()
  "Keep standard exact-match semantics when only point could move."
  (let* ((completion-styles '(nucleo))
         (table '("foo"))
         (md (completion-metadata "foo" table nil)))
    (should (eq (completion-try-completion "foo" table nil 2 md)
                t))))

(ert-deftest nucleo-completion-try-table-terminator-test ()
  "Honor table-native `try-completion' finalization such as terminators."
  (let* ((completion-styles '(nucleo))
         (table (apply-partially #'completion-table-with-terminator
                                 "/"
                                 '("foo")))
         (md (completion-metadata "foo" table nil)))
    (should (equal (completion-try-completion "foo" table nil 3 md)
                   '("foo/" . 4))))
  (let* ((completion-styles '(nucleo))
         (table (apply-partially #'completion-table-with-terminator
                                 "/"
                                 '("foo-bar")))
         (md (completion-metadata "fb" table nil)))
    (should (equal (completion-try-completion "fb" table nil 2 md)
                   '("foo-bar/" . 8)))))

(ert-deftest nucleo-completion-try-table-terminator-after-point-test ()
  "Merge a table terminator with matching text already after point."
  (let* ((completion-styles '(nucleo))
         (table (apply-partially #'completion-table-with-terminator
                                 "/"
                                 '("foo")))
         (md (completion-metadata "fo/" table nil)))
    (should (equal (completion-try-completion "fo/" table nil 2 md)
                   '("foo/" . 4)))))

(ert-deftest nucleo-completion-language-expander-selection-test ()
  (let ((nucleo-completion-languages '(japanese korean))
        (nucleo-completion-regexp-functions (list (lambda (_) "custom"))))
    (cl-letf (((symbol-function 'nucleo-completion-migemo-regexp)
               (lambda (_) "日本"))
              ((symbol-function 'nucleo-completion-pinyin-regexp)
               (lambda (_) "中国"))
              ((symbol-function 'nucleo-completion-hangul-regexp)
               (lambda (_) "감사")))
      (should (equal (nucleo-completion--regexp-function-regexps "abc")
                     '("custom" "日本" "감사")))
      (with-temp-buffer
        (setq-local nucleo-completion-languages nil)
        (should (equal (nucleo-completion--regexp-function-regexps "abc")
                       '("custom")))))))

(ert-deftest nucleo-completion-language-missing-function-test ()
  (let ((nucleo-completion-languages '(japanese chinese korean))
        (nucleo-completion-regexp-functions nil)
        (original-fboundp (symbol-function 'fboundp)))
    (cl-letf (((symbol-function 'fboundp)
               (lambda (function)
                 (unless (memq function '(nucleo-completion-migemo-regexp
                                          nucleo-completion-pinyin-regexp
                                          nucleo-completion-hangul-regexp))
                   (funcall original-fboundp function)))))
      (should-not (nucleo-completion--regexp-expanders))
      (should-not (nucleo-completion--regexp-function-regexps "abc")))))

(ert-deftest nucleo-completion-context-snapshot-test ()
  "Keep source identity and snapshot mutable list settings for reuse."
  (let* ((table (list "candidate"))
         (predicate (list 'lambda '(candidate) t))
         (regexps (list "first"))
         (nucleo-completion-regexp-functions (list 'ignore))
         (nucleo-completion-languages (list 'japanese))
         (context (nucleo-completion--completion-context table predicate regexps)))
    (should (nucleo-completion--completion-context-matches-p
             context table predicate (copy-sequence regexps)))
    (should-not (nucleo-completion--completion-context-matches-p
                 context (copy-sequence table) predicate regexps))
    (should-not (nucleo-completion--completion-context-matches-p
                 context table (copy-tree predicate) regexps))
    (should-not (nucleo-completion--completion-context-matches-p
                 nil table predicate regexps))
    (setcar regexps "changed")
    (should-not (nucleo-completion--completion-context-matches-p
                 context table predicate regexps))
    (setcar regexps "first")
    (setcar nucleo-completion-regexp-functions 'identity)
    (should-not (nucleo-completion--completion-context-matches-p
                 context table predicate regexps))
    (setcar nucleo-completion-regexp-functions 'ignore)
    (setcar nucleo-completion-languages 'chinese)
    (should-not (nucleo-completion--completion-context-matches-p
                 context table predicate regexps))))

(ert-deftest nucleo-completion-language-cache-and-context-test ()
  (let ((nucleo-completion-languages '(japanese))
        (nucleo-completion-regexp-functions nil)
        (table '("日本"))
        (available nil)
        (original-fboundp (symbol-function 'fboundp)))
    (cl-letf (((symbol-function 'nucleo-completion-migemo-regexp)
               (lambda (_) "日本"))
              ((symbol-function 'nucleo-completion-pinyin-regexp)
               (lambda (_) "中国"))
              ((symbol-function 'fboundp)
               (lambda (function)
                 (if (eq function 'nucleo-completion-migemo-regexp)
                     available
                   (funcall original-fboundp function)))))
      (nucleo-completion--with-completion-pass-caches
        (let ((context (nucleo-completion-tests--context table)))
          (should-not (nucleo-completion--regexp-function-regexps "abc"))
          (setq available t)
          (should-not (nucleo-completion--completion-context-matches-p
                       context table nil nil))
          (should (equal (nucleo-completion--regexp-function-regexps "abc")
                         '("日本")))
          (setq context (nucleo-completion-tests--context table)
                nucleo-completion-languages '(chinese))
          (should-not (nucleo-completion--completion-context-matches-p
                       context table nil nil))
          (should (equal (nucleo-completion--regexp-function-regexps "abc")
                         '("中国"))))))))

(ert-deftest nucleo-completion-language-completion-paths-test ()
  (let ((nucleo-completion-languages '(japanese))
        (nucleo-completion-regexp-functions
         (list (lambda (term) (when (equal term "go") "語"))))
        (completion-ignore-case nil))
    (cl-letf (((symbol-function 'nucleo-completion-migemo-regexp)
               (lambda (term) (when (equal term "nihon") "日本"))))
      (should (equal (nucleo-completion-tests--plain
                      (nucleo-completion-all-completions
                       "nihon go" '("日本語" "日本史")))
                     '("日本語")))
      (should (equal (nucleo-completion-try-completion
                      "nihon go" '("日本語") nil 8)
                     '("日本語" . 3)))
      (let ((candidate (copy-sequence "日本語")))
        (nucleo-completion-highlight "nihon go" candidate '(0))
        (should (get-text-property 1 'face candidate))
        (should (get-text-property 2 'face candidate))))))

(ert-deftest nucleo-completion-native-language-smoke-test ()
  (skip-unless (and (fboundp 'nucleo-completion-migemo-regexp)
                    (fboundp 'nucleo-completion-pinyin-regexp)
                    (fboundp 'nucleo-completion-hangul-regexp)))
  (dolist (case '((nucleo-completion-migemo-regexp "kensaku" "検索")
                  (nucleo-completion-migemo-regexp "nihon" "日本")
                  (nucleo-completion-pinyin-regexp "zhongguo" "中国")
                  (nucleo-completion-pinyin-regexp "xian" "西安")
                  (nucleo-completion-pinyin-regexp "xian" "先")
                  (nucleo-completion-pinyin-regexp "zhongg" "中国")
                  (nucleo-completion-hangul-regexp "ㄱㅅ" "감사")
                  (nucleo-completion-hangul-regexp "가ᄂ" "가나")))
    (let ((regexp (funcall (nth 0 case) (nth 1 case))))
      (should (and (stringp regexp) (not (string-empty-p regexp))))
      (should (string-match-p regexp (nth 2 case)))))
  (dolist (function '(nucleo-completion-migemo-regexp
                      nucleo-completion-pinyin-regexp
                      nucleo-completion-hangul-regexp))
    (should (member (funcall function "a*") '(nil "")))))

(ert-deftest nucleo-completion-native-language-completion-test ()
  (skip-unless (and (fboundp 'nucleo-completion-migemo-regexp)
                    (fboundp 'nucleo-completion-pinyin-regexp)
                    (fboundp 'nucleo-completion-hangul-regexp)))
  (let ((nucleo-completion--native-regexp-minimum-candidates 1)
        (nucleo-completion-languages '(japanese chinese korean))
        (nucleo-completion-regexp-functions nil))
    (dolist (case '(("nihon txt" "日本.txt" "日本.org")
                    ("zhongg" "中国" "日本")
                    ("ㄱㅅ" "감사" "나무")
                    ("가ᄂ" "가나" "나무")))
      (should (equal (nucleo-completion-tests--plain
                      (nucleo-completion-all-completions
                       (nth 0 case) (list (nth 1 case) (nth 2 case))))
                     (list (nth 1 case)))))))

(ert-deftest nucleo-completion-native-language-lazy-priority-test ()
  (skip-unless (and (fboundp 'nucleo-completion-migemo-regexp)
                    (nucleo-completion--module-ready-p)))
  (let ((nucleo-completion--native-regexp-minimum-candidates 1)
        (nucleo-completion-languages '(japanese))
        (nucleo-completion-regexp-functions nil)
        (completion-lazy-hilit t)
        (nucleo-completion-highlight-score-bands t)
        (nucleo-completion-max-highlighted-completions 10))
    (dolist (priority '(before after))
      (let* ((nucleo-completion-regexp-only-match-priority priority)
             (all (nucleo-completion-all-completions
                   "nihon" '("nihon" "日本")))
             (language-match (cl-find "日本" all :test #'equal))
             (highlighted (funcall completion-lazy-hilit-fn
                                   (copy-sequence language-match))))
        (should (equal (nucleo-completion-tests--plain all)
                       (if (eq priority 'before)
                           '("日本" "nihon")
                         '("nihon" "日本"))))
        (should (nucleo-completion--candidate-score language-match))
        (should (nucleo-completion-tests--high-score-face-p
                 (get-text-property 0 'face highlighted)))
        (should (memq 'completions-common-part
                      (ensure-list (get-text-property 0 'face highlighted))))))))

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

(ert-deftest nucleo-completion-try-regexp-function-test ()
  (let* ((completion-styles '(nucleo))
         (nucleo-completion-regexp-functions
          (list (lambda (term)
                  (when (string= term "nihon")
                    "日本"))))
         (table '("日本語" "nihon-go"))
         (md (completion-metadata "nihon" table nil)))
    (should (equal (completion-try-completion "nihon" table nil 5 md)
                   '("nihon" . 5))))
  (let* ((completion-styles '(nucleo))
         (nucleo-completion-regexp-functions
          (list (lambda (term)
                  (when (string= term "nihon")
                    "日本"))))
         (table '("日本語"))
         (md (completion-metadata "nihon" table nil)))
    (should (equal (completion-try-completion "nihon" table nil 5 md)
                   '("日本語" . 3)))))

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

(ert-deftest nucleo-completion-regexp-function-empty-regexp-test ()
  (let ((nucleo-completion-regexp-functions
         (list (lambda (_term) ""))))
    (should-not (nucleo-completion--regexp-function-regexps "zz"))
    (should-not (nucleo-completion-all-completions
                 "zz" '("foo" "bar") nil nil))))

(ert-deftest nucleo-completion-regexp-function-error-warning-test ()
  (let ((expander (lambda (_term)
                    (error "Regexp backend failed"))))
    (let ((nucleo-completion-regexp-functions (list expander))
          (nucleo-completion-report-regexp-function-errors nil)
          (nucleo-completion--regexp-function-error-warnings nil))
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (&rest _)
                   (error "Regexp warning should be disabled"))))
        (should-not (nucleo-completion--regexp-function-regexps "fail"))))
    (let ((nucleo-completion-regexp-functions (list expander))
          (nucleo-completion-report-regexp-function-errors t)
          (nucleo-completion--regexp-function-error-warnings nil)
          warnings)
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (type message &optional level buffer-name)
                   (push (list type message level buffer-name) warnings))))
        (should-not (nucleo-completion--regexp-function-regexps "fail"))
        (should-not (nucleo-completion--regexp-function-regexps "again"))
        (should (= (length warnings) 1))
        (should (eq (caar warnings) 'nucleo-completion))
        (should (string-match-p "Regexp backend failed"
                                (cadar warnings)))))))

(ert-deftest nucleo-completion-regexp-functions-skip-short-terms-test ()
  (let ((nucleo-completion-tests--regexp-calls 0)
        (nucleo-completion-regexp-minimum-term-length 2)
        (nucleo-completion-regexp-functions
         (list (lambda (_term)
                 (setq nucleo-completion-tests--regexp-calls
                       (1+ nucleo-completion-tests--regexp-calls))
                 "日本"))))
    (should-not (nucleo-completion--regexp-function-regexps "n"))
    (should (= nucleo-completion-tests--regexp-calls 0))
    (should (equal (nucleo-completion--regexp-function-regexps "ni")
                   '("日本")))
    (should (= nucleo-completion-tests--regexp-calls 1)))
  (let ((nucleo-completion-tests--regexp-calls 0)
        (nucleo-completion-regexp-minimum-term-length 1)
        (nucleo-completion-regexp-functions
         (list (lambda (_term)
                 (setq nucleo-completion-tests--regexp-calls
                       (1+ nucleo-completion-tests--regexp-calls))
                 "日本"))))
    (should (equal (nucleo-completion--regexp-function-regexps "n")
                   '("日本")))
    (should (= nucleo-completion-tests--regexp-calls 1))))

(ert-deftest nucleo-completion-expanded-regexp-skips-terms-without-functions-test ()
  (let ((nucleo-completion-regexp-functions nil))
    (cl-letf (((symbol-function 'nucleo-completion--terms)
               (lambda (_pattern)
                 (error "No regexp functions means no term expansion"))))
      (should-not (nucleo-completion--expanded-regexp-p "nihon")))))

(ert-deftest nucleo-completion-term-regexps-skips-expander-without-functions-test ()
  (let ((nucleo-completion-regexp-functions nil))
    (cl-letf (((symbol-function 'nucleo-completion--regexp-function-regexps)
               (lambda (_term)
                 (error "No regexp functions means no expander lookup"))))
      (should (equal (nucleo-completion--term-regexps "fb")
                     (list (concat "\\`"
                                   (nucleo-completion--subsequence-regexp
                                    "fb"))))))))

(ert-deftest nucleo-completion-regexp-list-skips-groups-without-functions-test ()
  (let ((nucleo-completion-regexp-functions nil)
        completion-regexp-list)
    (cl-letf (((symbol-function 'nucleo-completion--term-regexp-groups)
               (lambda (_needle)
                 (error "No regexp functions means no regexp groups"))))
      (should (equal (nucleo-completion--completion-regexp-list
                      "fb bz" nil nil)
                     (list (concat "\\`"
                                   (nucleo-completion--subsequence-regexp
                                    "fb"))
                           (concat "\\`"
                                   (nucleo-completion--subsequence-regexp
                                    "bz"))))))))

(ert-deftest nucleo-completion-regexp-only-groups-skip-expanded-fuzzy-test ()
  (let ((nucleo-completion-regexp-functions
         (list (lambda (term)
                 (when (string= term "nihon")
                   "日本")))))
    (should (equal (nucleo-completion--regexp-only-regexp-groups "nihon go")
                   (list '("日本")
                         (list (concat "\\`"
                                       (nucleo-completion--subsequence-regexp
                                        "go"))))))))

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

(ert-deftest nucleo-completion-regexp-functions-not-cached-between-completions-test ()
  (let ((nucleo-completion-tests--regexp-calls 0)
        (nucleo-completion-regexp-functions
         (list #'nucleo-completion-tests--nihon-regexp)))
    (cl-letf (((symbol-function 'nucleo-completion--module-ready-p)
               (lambda () nil)))
      (nucleo-completion-all-completions
       "nihon" '("日本語" "nihon-go" "英語"))
      (nucleo-completion-all-completions
       "nihon" '("日本語" "nihon-go" "英語"))
      (should (= nucleo-completion-tests--regexp-calls 2)))))

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
      (should (equal (nucleo-completion-tests--plain
                      (nucleo-completion-all-completions
                       "nihon" '("日本語" "roman-nihon" "日本史")))
                     '("日本語" "日本史" "roman-nihon"))))))

(ert-deftest nucleo-completion-sort-with-module-appends-ascii-regexp-only-matches-test ()
  (let ((nucleo-completion-regexp-functions
         (list (lambda (term)
                 (when (string= term "ba")
                   "readable")))))
    (cl-letf (((symbol-function 'nucleo-completion-candidates)
               (lambda (_needle _candidates _ignore-case _by-length
                                _alphabetically _limit
                                &optional return-all-scores)
                 (nucleo-completion-tests--bundle
                  '(("org-babel-execute" 128 nil)
                    ("org-table-align" 96 nil))
                  return-all-scores))))
      (should (equal (nucleo-completion-tests--plain
                      (nucleo-completion-all-completions
                       "org ba"
                       '("save-place-forget-unreadable-files"
                         "org-babel-execute"
                         "org-table-align")))
                     '("org-babel-execute"
                       "org-table-align"
                       "save-place-forget-unreadable-files"))))))

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

(ert-deftest nucleo-completion-module-skips-regexp-only-pass-when-complete-test ()
  (let ((nucleo-completion-regexp-functions
         (list (lambda (term)
                 (when (string= term "nihon")
                   "日本")))))
    (cl-letf (((symbol-function 'nucleo-completion-candidates)
               (lambda (_needle candidates _ignore-case _by-length
                                _alphabetically _limit
                                &optional return-all-scores)
                 (nucleo-completion-tests--bundle
                  (mapcar (lambda (candidate)
                            (list candidate 128 nil))
                          candidates)
                  return-all-scores)))
              ((symbol-function 'nucleo-completion--regexp-match-p)
               (lambda (&rest _)
                 (error "complete module results need no regexp-only pass")))
              ((symbol-function 'nucleo-completion--regexp-only-regexp-groups)
               (lambda (&rest _)
                 (error "complete module results need no regexp-only regexps"))))
      (should (equal (nucleo-completion-tests--plain
                      (nucleo-completion-all-completions
                       "nihon" '("roman-nihon" "nihon-roman")))
                     '("roman-nihon" "nihon-roman"))))))

(ert-deftest nucleo-completion-regexp-only-empty-module-skips-seen-table-test ()
  (let ((nucleo-completion-regexp-functions
         (list (lambda (term)
                 (when (string= term "nihon")
                   "日本")))))
    (cl-letf (((symbol-function 'make-hash-table)
               (lambda (&rest _)
                 (error "Empty module results need no seen table"))))
      (should (equal (nucleo-completion--regexp-only-candidates
                      "nihon" '("roman-nihon" "日本" "miss") nil)
                     '("日本"))))))

(ert-deftest nucleo-completion-regexp-only-seen-table-skips-size-scan-test ()
  (let ((nucleo-completion-regexp-functions
         (list (lambda (term)
                 (when (string= term "nihon")
                   "日本"))))
        (original-make-hash-table (symbol-function 'make-hash-table)))
    (cl-letf (((symbol-function 'make-hash-table)
               (lambda (&rest args)
                 (should-not (memq :size args))
                 (apply original-make-hash-table args))))
      (should (equal (nucleo-completion--regexp-only-candidates
                      "nihon"
                      '("roman-nihon" "日本" "miss")
                      '("roman-nihon"))
                     '("日本"))))))

(ert-deftest nucleo-completion-regexp-only-merge-skips-work-without-extra-test ()
  (let ((bundle '(("roman-nihon") (("roman-nihon" 128 nil)) (128))))
    (cl-letf (((symbol-function 'nucleo-completion--regexp-only-candidates)
               (lambda (&rest _)
                 nil))
              ((symbol-function 'nucleo-completion--max-score)
               (lambda (&rest _)
                 (error "No regexp-only matches need no score lookup")))
              ((symbol-function 'nucleo-completion--split-regexp-only-candidates)
               (lambda (&rest _)
                 (error "No regexp-only matches need no split"))))
      (should (eq (nucleo-completion--merge-regexp-only-matches
                   "nihon" '("roman-nihon" "日本") bundle t)
                  bundle)))))

(ert-deftest nucleo-completion-regexp-only-merge-skips-unused-score-test ()
  (let ((nucleo-completion-highlight-score-bands nil)
        (bundle '(("roman-nihon") nil nil)))
    (cl-letf (((symbol-function 'nucleo-completion--regexp-only-candidates)
               (lambda (&rest _)
                 '("日本")))
              ((symbol-function 'nucleo-completion--max-score)
               (lambda (&rest _)
                 (error "Regexp-only merge needs no unused score"))))
      (pcase-let ((`(,candidates ,top-info ,full-scores)
                   (nucleo-completion--merge-regexp-only-matches
                    "nihon" '("roman-nihon" "日本") bundle nil)))
        (should (equal candidates '("日本" "roman-nihon")))
        (should-not top-info)
        (should-not full-scores)))))

(ert-deftest nucleo-completion-regexp-only-skips-score-property-without-score-band-test ()
  (let ((nucleo-completion-highlight-score-bands nil)
        (bundle '(("roman-nihon") (("roman-nihon" 128 nil)) (128))))
    (cl-letf (((symbol-function 'nucleo-completion--regexp-only-candidates)
               (lambda (&rest _)
                 '("日本")))
              ((symbol-function 'nucleo-completion--propertize-score)
               (lambda (&rest _)
                 (error "Score properties are only needed for score bands"))))
      (pcase-let ((`(,candidates ,_top-info ,full-scores)
                   (nucleo-completion--merge-regexp-only-matches
                    "nihon" '("roman-nihon" "日本") bundle nil)))
        (should (equal candidates '("日本" "roman-nihon")))
        (should (equal full-scores '(128 128)))))))

(ert-deftest nucleo-completion-regexp-only-score-candidates-test ()
  (pcase-let ((`(,candidates . ,scores)
               (nucleo-completion--regexp-only-score-candidates
                '("日本" "仮名") 7 t)))
    (should (equal (nucleo-completion-tests--plain candidates)
                   '("日本" "仮名")))
    (should (equal (nucleo-completion--candidate-score (car candidates))
                   7))
    (should (equal (nucleo-completion--candidate-score (cadr candidates))
                   7))
    (should (equal scores '(7 7))))
  (let ((original '("日本" "仮名")))
    (pcase-let ((`(,candidates . ,scores)
                 (nucleo-completion--regexp-only-score-candidates
                  original 7 nil)))
      (should (eq candidates original))
      (should (equal scores '(7 7))))))

(ert-deftest nucleo-completion-regexp-only-lazy-score-band-skips-top-info-test ()
  (let ((completion-ignore-case nil)
        (completion-lazy-hilit t)
        (nucleo-completion-highlight-score-bands t)
        (nucleo-completion-max-highlighted-completions 10)
        (nucleo-completion-regexp-functions
         (list (lambda (term)
                 (when (string= term "nihon")
                   "日本")))))
    (cl-letf (((symbol-function 'nucleo-completion-candidates)
               (lambda (_needle _candidates _ignore-case _by-length
                                _alphabetically _limit
                                &optional return-all-scores)
                 (nucleo-completion-tests--bundle
                  '(("roman-nihon" 128 nil))
                  return-all-scores)))
              ((symbol-function 'nucleo-completion--regexp-only-top-info)
               (lambda (&rest _)
                 (error "Lazy score-band uses score properties"))))
      (let* ((all (nucleo-completion-all-completions
                   "nihon" '("roman-nihon" "日本")))
             (highlighted (funcall completion-lazy-hilit-fn
                                   (copy-sequence (car all)))))
        (should (equal (nucleo-completion-tests--plain all)
                       '("日本" "roman-nihon")))
        (should (nucleo-completion--candidate-score (car all)))
        (should (nucleo-completion-tests--high-score-face-p
                 (get-text-property 0 'face highlighted)))))))

(ert-deftest nucleo-completion-regexp-only-skips-top-info-when-unhighlighted-test ()
  (let ((nucleo-completion-max-highlighted-completions 0)
        (nucleo-completion-regexp-functions
         (list (lambda (term)
                 (when (string= term "nihon")
                   "日本")))))
    (cl-letf (((symbol-function 'nucleo-completion-candidates)
               (lambda (_needle _candidates _ignore-case _by-length
                                _alphabetically _limit
                                &optional return-all-scores)
                 (nucleo-completion-tests--bundle
                  '(("roman-nihon" 128 nil))
                  return-all-scores)))
              ((symbol-function 'nucleo-completion--regexp-only-top-info)
               (lambda (&rest _)
                 (error "Unhighlighted regexp-only matches need no top-info"))))
      (should (equal (nucleo-completion-tests--plain
                      (nucleo-completion-all-completions
                       "nihon" '("roman-nihon" "日本")))
                     '("日本" "roman-nihon"))))))

(ert-deftest nucleo-completion-regexp-only-skips-top-info-without-score-band-test ()
  (let ((nucleo-completion-highlight-score-bands nil)
        (nucleo-completion-max-highlighted-completions 10)
        (nucleo-completion-regexp-functions
         (list (lambda (term)
                 (when (string= term "nihon")
                   "日本")))))
    (cl-letf (((symbol-function 'nucleo-completion-candidates)
               (lambda (_needle _candidates _ignore-case _by-length
                                _alphabetically _limit
                                &optional return-all-scores)
                 (nucleo-completion-tests--bundle
                  '(("roman-nihon" 128 nil))
                  return-all-scores)))
              ((symbol-function 'nucleo-completion--regexp-only-top-info)
               (lambda (&rest _)
                 (error "Regexp-only top-info is only needed for score bands"))))
      (should (equal (nucleo-completion-tests--plain
                      (nucleo-completion-all-completions
                       "nihon" '("roman-nihon" "日本")))
                     '("日本" "roman-nihon"))))))

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
      (should (equal (nucleo-completion-tests--plain
                      (nucleo-completion-all-completions
                       "nihon" '("roman-nihon" "nihon-tail" "日本")))
                     '("日本" "roman-nihon" "nihon-tail"))))))

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

(ert-deftest nucleo-completion-history-ranking-strips-prefix-test ()
  (let ((nucleo-completion-sort-ties-by-history t)
        (minibuffer-history-variable 'nucleo-completion-tests-history)
        (nucleo-completion-tests-history
         '("/tmp/alpha" "/var/beta" "/tmp/gamma")))
    (cl-letf (((symbol-function 'minibufferp)
               (lambda (&rest _) t)))
      (should (equal (car (nucleo-completion--history-ranking
                           "/tmp/" '("beta" "gamma" "alpha")))
                     '(nil 1 0))))))

(ert-deftest nucleo-completion-history-ranking-uses-equal-string-key-test ()
  (let ((nucleo-completion-sort-ties-by-history t)
        (rank-table (make-hash-table :test #'equal))
        (candidate (propertize "alpha" 'face 'bold)))
    (puthash "alpha" 0 rank-table)
    (cl-letf (((symbol-function 'nucleo-completion--history-rank-table)
               (lambda (_prefix) rank-table))
              ((symbol-function 'substring-no-properties)
               (lambda (&rest _)
                 (error "History lookup should not copy candidate keys"))))
      (should (equal (car (nucleo-completion--history-ranking
                           "" (list candidate "beta")))
                     '(0 nil))))))

(ert-deftest nucleo-completion-history-ranking-noops-outside-minibuffer-test ()
  (let ((nucleo-completion-sort-ties-by-history t)
        (minibuffer-history-variable 'nucleo-completion-tests-history)
        (nucleo-completion-tests-history '("alpha")))
    (cl-letf (((symbol-function 'minibufferp)
               (lambda (&rest _) nil)))
      (should-not (nucleo-completion--history-ranking
                   "" '("alpha"))))))

(ert-deftest nucleo-completion-history-ranking-skips-singleton-test ()
  (let ((nucleo-completion-sort-ties-by-history t))
    (cl-letf (((symbol-function 'nucleo-completion--history-rank-table)
               (lambda (_prefix)
                 (error "singleton results do not need history ranks"))))
      (should-not (nucleo-completion--history-ranking "" nil))
      (should-not (nucleo-completion--history-ranking "" '("alpha"))))))

(ert-deftest nucleo-completion-history-rank-table-skips-empty-entries-test ()
  (cl-letf (((symbol-function 'make-hash-table)
             (lambda (&rest _)
               (error "Empty history entries need no hash table"))))
    (should-not (nucleo-completion--history-rank-table-from-entries nil))
    (should-not (nucleo-completion--history-rank-table-from-entries nil ""))))

(ert-deftest nucleo-completion-history-rank-table-skips-unusable-entries-test ()
  (cl-letf (((symbol-function 'make-hash-table)
             (lambda (&rest _)
               (error "Unusable history entries need no hash table"))))
    (should-not
     (nucleo-completion--history-rank-table-from-entries
      '(nil 42 "/var/beta")
      "/tmp/"))))

(ert-deftest nucleo-completion-history-rank-table-skips-size-scan-test ()
  (let ((original-make-hash-table (symbol-function 'make-hash-table)))
    (cl-letf (((symbol-function 'make-hash-table)
               (lambda (&rest args)
                 (should-not (memq :size args))
                 (apply original-make-hash-table args))))
      (should (nucleo-completion--history-rank-table-from-entries
               '("alpha" "beta"))))))

(ert-deftest nucleo-completion-history-ranking-uses-corfu-history-test ()
  (let ((nucleo-completion-sort-ties-by-history t)
        (corfu-history-mode t)
        (corfu-history '("alpha" "beta")))
    (cl-letf (((symbol-function 'minibufferp)
               (lambda (&rest _) nil)))
      (should (equal (car (nucleo-completion--history-ranking
                           "" (list "beta"
                                    (propertize "alpha" 'face 'bold)
                                    "aardvark")))
                     '(1 0 nil))))))

(ert-deftest nucleo-completion-sort-ties-by-history-test ()
  (let ((nucleo-completion-sort-ties-by-history t)
        (nucleo-completion-sort-ties-by-length nil)
        (nucleo-completion-sort-ties-alphabetically nil)
        (minibuffer-history-variable 'nucleo-completion-tests-history)
        (nucleo-completion-tests-history '("alpha" "beta")))
    (cl-letf (((symbol-function 'nucleo-completion--native-completion-results)
               (lambda (&rest _) 'nucleo-completion-unsupported-regexp))
              ((symbol-function 'nucleo-completion--module-supports-history-p)
               (lambda () t))
              ((symbol-function 'minibufferp)
               (lambda (&rest _) t))
              ((symbol-function 'nucleo-completion-candidates-with-history)
               (lambda (needle candidates ignore-case by-length alphabetically
                               history-ranks limit &optional return-all-scores)
                 (should (equal needle "a"))
                 (should (equal candidates '("beta" "alpha" "aardvark")))
                 (should-not ignore-case)
                 (should-not by-length)
                 (should-not alphabetically)
                 (should (equal history-ranks '(1 0 nil)))
                 (should (= limit 0))
                 (nucleo-completion-tests--bundle
                  '(("alpha" 10 nil) ("beta" 10 nil) ("aardvark" 9 nil))
                  return-all-scores))))
      (should (equal (nucleo-completion--module-filter
                      "a" '("beta" "alpha" "aardvark") nil)
                     '("alpha" "beta" "aardvark"))))))

(ert-deftest nucleo-completion-sort-ties-by-corfu-history-test ()
  (let ((nucleo-completion-sort-ties-by-history t)
        (nucleo-completion-sort-ties-by-length nil)
        (nucleo-completion-sort-ties-alphabetically nil)
        (corfu-history-mode t)
        (corfu-history '("alpha" "beta")))
    (cl-letf (((symbol-function 'nucleo-completion--native-completion-results)
               (lambda (&rest _) 'nucleo-completion-unsupported-regexp))
              ((symbol-function 'nucleo-completion--module-supports-history-p)
               (lambda () t))
              ((symbol-function 'minibufferp)
               (lambda (&rest _) nil))
              ((symbol-function 'nucleo-completion-candidates)
               (lambda (&rest _)
                 (error "Expected history-aware module call")))
              ((symbol-function 'nucleo-completion-candidates-with-history)
               (lambda (needle candidates ignore-case by-length alphabetically
                               history-ranks limit &optional return-all-scores)
                 (should (equal needle "a"))
                 (should (equal candidates '("beta" "alpha" "aardvark")))
                 (should-not ignore-case)
                 (should-not by-length)
                 (should-not alphabetically)
                 (should (equal history-ranks '(1 0 nil)))
                 (should (= limit 0))
                 (nucleo-completion-tests--bundle
                  '(("alpha" 10 nil) ("beta" 10 nil) ("aardvark" 9 nil))
                  return-all-scores))))
      (should (equal (nucleo-completion--module-filter
                      "a" '("beta" "alpha" "aardvark") nil)
                     '("alpha" "beta" "aardvark"))))))

(ert-deftest nucleo-completion-sort-ties-by-history-old-module-fallback-test ()
  (let ((nucleo-completion-sort-ties-by-history t)
        (nucleo-completion-sort-ties-by-length nil)
        (nucleo-completion-sort-ties-alphabetically nil)
        (minibuffer-history-variable 'nucleo-completion-tests-history)
        (nucleo-completion-tests-history '("aardvark" "alpha" "beta")))
    (cl-letf (((symbol-function 'nucleo-completion--native-completion-results)
               (lambda (&rest _) 'nucleo-completion-unsupported-regexp))
              ((symbol-function 'nucleo-completion--module-supports-history-p)
               (lambda () nil))
              ((symbol-function 'minibufferp)
               (lambda (&rest _) t))
              ((symbol-function 'nucleo-completion-candidates)
               (lambda (needle candidates ignore-case by-length alphabetically
                               limit &optional return-all-scores)
                 (should (equal needle "a"))
                 (should (equal candidates '("beta" "alpha" "aardvark")))
                 (should-not ignore-case)
                 (should-not by-length)
                 (should-not alphabetically)
                 (should (= limit 0))
                 (should return-all-scores)
                 (nucleo-completion-tests--bundle
                  '(("beta" 10 nil) ("alpha" 10 nil) ("aardvark" 9 nil))
                  return-all-scores))))
      (should (equal (nucleo-completion--module-filter
                      "a" '("beta" "alpha" "aardvark") nil)
                     '("alpha" "beta" "aardvark"))))))

(ert-deftest nucleo-completion-sort-bundle-history-skips-unique-scores-test ()
  (let ((rank-table (make-hash-table :test #'equal))
        (bundle '(("beta" "alpha" "aardvark")
                  (("beta" 30 nil) ("alpha" 20 nil) ("aardvark" 10 nil))
                  (30 20 10))))
    (puthash "aardvark" 0 rank-table)
    (puthash "alpha" 1 rank-table)
    (puthash "beta" 2 rank-table)
    (cl-letf (((symbol-function 'sort)
               (lambda (&rest _)
                 (error "Unique scores need no history tie sort"))))
      (should (eq (nucleo-completion--sort-bundle-ties-by-history
                   bundle rank-table)
                  bundle)))))

(ert-deftest nucleo-completion-sort-top-info-history-skips-unique-scores-test ()
  (let ((rank-table (make-hash-table :test #'equal))
        (top-info '(("beta" 30 nil) ("alpha" 20 nil) ("aardvark" 10 nil))))
    (puthash "aardvark" 0 rank-table)
    (puthash "alpha" 1 rank-table)
    (puthash "beta" 2 rank-table)
    (cl-letf (((symbol-function 'sort)
               (lambda (&rest _)
                 (error "Unique top-info scores need no history tie sort"))))
      (should (eq (nucleo-completion--sort-top-info-ties-by-history
                   top-info rank-table)
                  top-info)))))

(ert-deftest nucleo-completion-sort-bundle-history-keeps-short-score-extra-test ()
  (let ((rank-table (make-hash-table :test #'equal))
        (bundle '(("beta" "alpha" "aardvark") nil (10 10))))
    (puthash "alpha" 0 rank-table)
    (puthash "beta" 1 rank-table)
    (should (equal (nucleo-completion--sort-bundle-ties-by-history
                    bundle rank-table)
                   '(("alpha" "beta" "aardvark") nil (10 10 nil))))))

(ert-deftest nucleo-completion-sort-bundle-history-sorts-missing-score-ties-test ()
  (let ((rank-table (make-hash-table :test #'equal))
        (bundle '(("beta" "alpha" "zeta" "aardvark")
                  nil
                  (30 20))))
    (puthash "aardvark" 0 rank-table)
    (puthash "zeta" 1 rank-table)
    (should (equal (nucleo-completion--sort-bundle-ties-by-history
                    bundle rank-table)
                   '(("beta" "alpha" "aardvark" "zeta")
                     nil
                     (30 20 nil nil))))))

(ert-deftest nucleo-completion-history-sort-before-reuses-rank-lookups-test ()
  (let ((rank-table (make-hash-table :test #'equal))
        (calls 0)
        (original-key
         (symbol-function 'nucleo-completion--history-candidate-key)))
    (puthash "alpha" 1 rank-table)
    (puthash "beta" 0 rank-table)
    (cl-letf (((symbol-function 'nucleo-completion--history-candidate-key)
               (lambda (candidate)
                 (setq calls (1+ calls))
                 (funcall original-key candidate))))
      (should-not
       (nucleo-completion--history-sort-before-p
        "alpha" 10 0 "beta" 10 1 rank-table))
      (should (= calls 2)))))

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

(ert-deftest nucleo-completion-module-filter-with-scores-keeps-short-score-extra-test ()
  (cl-letf (((symbol-function 'nucleo-completion-candidates)
             (lambda (_needle _candidates _ignore-case _by-length
                              _alphabetically _limit
                              &optional return-all-scores)
               (should return-all-scores)
               '(("foo" "bar") nil (100)))))
    (should (equal (nucleo-completion--module-filter-with-scores
                    "f" '("foo" "bar") nil)
                   '(("foo" . 100) ("bar"))))))

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

(ert-deftest nucleo-completion-adjust-metadata-state-is-buffer-local-test ()
  (with-temp-buffer
    (setq nucleo-completion--filtering-p t)
    (should (eq (completion-metadata-get
                 (nucleo-completion-adjust-metadata '(metadata))
                 'display-sort-function)
                'identity)))
  (with-temp-buffer
    (should (equal (nucleo-completion-adjust-metadata
                    '(metadata (category . file)))
                   '(metadata (category . file))))))

(ert-deftest nucleo-completion-highlight-test ()
  (should (equal-including-properties
           (nucleo-completion-highlight "fb" "foo-baz")
           #("foo-baz" 0 1 (face completions-common-part)
             4 5 (face completions-common-part)))))

(ert-deftest nucleo-completion-highlight-splits-terms-once-test ()
  (let ((nucleo-completion-regexp-functions nil)
        (calls 0))
    (cl-letf (((symbol-function 'nucleo-completion--terms)
               (lambda (_needle)
                 (setq calls (1+ calls))
                 '("fb"))))
      (should (equal-including-properties
               (nucleo-completion-highlight "fb" (copy-sequence "foo-baz"))
               #("foo-baz" 0 1 (face completions-common-part)
                 4 5 (face completions-common-part))))
      (should (= calls 1)))))

(ert-deftest nucleo-completion-precomputed-highlight-test ()
  (should (equal-including-properties
           (nucleo-completion--highlight-candidate
            "fb" (copy-sequence "foo-baz") nil nil '(4 5))
           #("foo-baz" 4 6 (face completions-common-part)))))

(ert-deftest nucleo-completion-precomputed-highlight-ignores-invalid-indices-test ()
  (should (equal-including-properties
           (nucleo-completion--highlight-candidate
            "fb" (copy-sequence "foo-baz") nil nil '(-1 4 bad 99))
           #("foo-baz" 4 5 (face completions-common-part)))))

(ert-deftest nucleo-completion-precomputed-highlight-skips-terms-without-functions-test ()
  (let ((nucleo-completion-regexp-functions nil))
    (cl-letf (((symbol-function 'nucleo-completion--terms)
               (lambda (_needle)
                 (error "Precomputed fuzzy highlight needs no term split"))))
      (should (equal-including-properties
               (nucleo-completion-highlight
                "fb" (copy-sequence "foo-baz") '(0 4))
               #("foo-baz" 0 1 (face completions-common-part)
                 4 5 (face completions-common-part)))))))

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
                 (error "Top-info hash must not be built"))))
      (should (equal (nucleo-completion-tests--plain
                      (nucleo-completion-all-completions
                       "fo" '("foo" "fob" "bar")))
                     '("foo" "fob"))))))

(ert-deftest nucleo-completion-top-info-hash-skips-size-scan-test ()
  (let ((original-make-hash-table (symbol-function 'make-hash-table)))
    (cl-letf (((symbol-function 'make-hash-table)
               (lambda (&rest args)
                 (should-not (memq :size args))
                 (apply original-make-hash-table args))))
      (let ((table (nucleo-completion--top-info-hash
                    '(("foo" 100 nil) ("bar" 50 nil)))))
        (should (equal (nucleo-completion--top-info-score
                        (gethash "foo" table))
                       100))))))

(ert-deftest nucleo-completion-eager-score-band-skips-exact-cache-for-high-score-test ()
  (let ((completion-ignore-case nil)
        (completion-lazy-hilit nil)
        (nucleo-completion-highlight-score-bands t)
        (nucleo-completion-high-score-ratio 0.85)
        (nucleo-completion-max-highlighted-completions 10)
        (make-hash-table-calls 0)
        (original-make-hash-table (symbol-function 'make-hash-table)))
    (cl-letf (((symbol-function 'make-hash-table)
               (lambda (&rest args)
                 (setq make-hash-table-calls (1+ make-hash-table-calls))
                 (apply original-make-hash-table args)))
              ((symbol-function 'nucleo-completion--module-ready-p)
               (lambda () t))
              ((symbol-function 'nucleo-completion-candidates)
               (lambda (_needle _candidates _ignore-case _by-length
                                _alphabetically _limit
                                &optional return-all-scores)
                 (nucleo-completion-tests--bundle
                  '(("foo" 100 (0 1)))
                  return-all-scores))))
      (should (equal (nucleo-completion-tests--plain
                      (nucleo-completion-all-completions
                       "fo" '("foo" "bar")))
                     '("foo")))
      (should (= make-hash-table-calls 3)))))

(ert-deftest nucleo-completion-no-highlight-skips-exact-word-cache-test ()
  (let ((completion-ignore-case nil)
        (completion-lazy-hilit nil)
        (nucleo-completion-highlight-score-bands t)
        (nucleo-completion-max-highlighted-completions 0)
        (nucleo-completion-regexp-functions nil)
        (make-hash-table-calls 0)
        (original-make-hash-table (symbol-function 'make-hash-table)))
    (cl-letf (((symbol-function 'make-hash-table)
               (lambda (&rest args)
                 (setq make-hash-table-calls (1+ make-hash-table-calls))
                 (apply original-make-hash-table args)))
              ((symbol-function 'nucleo-completion--module-ready-p)
               (lambda () t))
              ((symbol-function 'nucleo-completion-candidates)
               (lambda (_needle _candidates _ignore-case _by-length
                                _alphabetically _limit
                                &optional return-all-scores)
                 (nucleo-completion-tests--bundle
                  '(("foo" 100 nil))
                  return-all-scores))))
      (should (equal (nucleo-completion-tests--plain
                      (nucleo-completion-all-completions
                       "fo" '("foo" "bar")))
                     '("foo")))
      (should (= make-hash-table-calls 2)))))

(ert-deftest nucleo-completion-no-result-skips-exact-word-cache-test ()
  (let ((completion-ignore-case nil)
        (completion-lazy-hilit nil)
        (nucleo-completion-highlight-score-bands t)
        (nucleo-completion-max-highlighted-completions 10)
        (nucleo-completion-regexp-functions nil)
        (make-hash-table-calls 0)
        (original-make-hash-table (symbol-function 'make-hash-table)))
    (cl-letf (((symbol-function 'make-hash-table)
               (lambda (&rest args)
                 (setq make-hash-table-calls (1+ make-hash-table-calls))
                 (apply original-make-hash-table args)))
              ((symbol-function 'nucleo-completion--module-ready-p)
               (lambda () t))
              ((symbol-function 'nucleo-completion-candidates)
               (lambda (&rest _)
                 '(nil nil nil))))
      (should-not (nucleo-completion-all-completions
                   "fo" '("foo" "bar")))
      (should (= make-hash-table-calls 2)))))

(ert-deftest nucleo-completion-lazy-highlight-avoids-key-allocation-test ()
  "Avoid allocating stripped keys during lazy highlighting.
Hash tables keyed on candidate strings rely on the `equal' test,
which already compares string contents independently of text
properties.  The lazy highlight lambda must therefore avoid
allocating a stripped key with `substring-no-properties' on each
invocation."
  (let ((completion-ignore-case nil)
        (completion-lazy-hilit t)
        (nucleo-completion-max-highlighted-completions 10)
        (calls 0))
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

(ert-deftest nucleo-completion-lazy-highlight-does-not-mutate-snapshot-test ()
  (let ((completion-ignore-case nil)
        (completion-lazy-hilit t)
        (nucleo-completion-max-highlighted-completions 10))
    (cl-letf (((symbol-function 'nucleo-completion--module-ready-p)
               (lambda () t))
              ((symbol-function 'nucleo-completion-candidates)
               (lambda (_needle _candidates _ignore-case _by-length
                                _alphabetically _limit
                                &optional return-all-scores)
                 (nucleo-completion-tests--bundle
                  '(("foo" 100 (0 1)) ("fob" 90 (0 1)))
                  return-all-scores))))
      (let* ((all (nucleo-completion-all-completions
                   "fo" '("foo" "fob" "bar") nil nil))
             (highlighted (funcall completion-lazy-hilit-fn (car all))))
        (should (get-text-property 0 'face highlighted))
        (should-not (get-text-property 0 'face (car all)))
        (should-not (get-text-property
                     0 'face (car nucleo-completion--current-result)))))))

(ert-deftest nucleo-completion-lazy-score-band-uses-score-property-test ()
  (let ((completion-ignore-case nil)
        (completion-lazy-hilit t)
        (nucleo-completion-highlight-score-bands t)
        (nucleo-completion-high-score-ratio 0.85)
        (nucleo-completion-max-highlighted-completions 1))
    (cl-letf (((symbol-function 'nucleo-completion-candidates)
               (lambda (_needle _candidates _ignore-case _by-length
                                _alphabetically _limit
                                &optional return-all-scores)
                 (list '("foo" "fob")
                       '(("foo" 100 (0 1)))
                       (when return-all-scores '(100 50))))))
      (let* ((all (nucleo-completion-all-completions
                   "fo" '("foo" "fob" "bar")))
             (highlighted (funcall completion-lazy-hilit-fn
                                   (copy-sequence (cadr all)))))
        (should (equal (nucleo-completion-tests--plain all)
                       '("foo" "fob")))
        (should (equal (nucleo-completion--candidate-score
                        (cadr all))
                       50))
        (should (memq 'nucleo-completion-low-score-face
                      (ensure-list
                       (get-text-property 0 'face highlighted))))))))

(ert-deftest nucleo-completion-lazy-score-band-uses-full-score-max-test ()
  (let ((completion-ignore-case nil)
        (completion-lazy-hilit t)
        (nucleo-completion-highlight-score-bands t)
        (nucleo-completion-high-score-ratio 0.85)
        (nucleo-completion-max-highlighted-completions 0))
    (cl-letf (((symbol-function 'nucleo-completion--module-ready-p)
               (lambda () t))
              ((symbol-function 'nucleo-completion-candidates)
               (lambda (_needle _candidates _ignore-case _by-length
                                _alphabetically _limit
                                &optional return-all-scores)
                 (list '("foo" "fob")
                       nil
                       (when return-all-scores '(100 50))))))
      (let* ((all (nucleo-completion-all-completions
                   "fo" '("foo" "fob" "bar")))
             (highlighted (funcall completion-lazy-hilit-fn
                                   (copy-sequence (car all)))))
        (should (equal (nucleo-completion-tests--plain all)
                       '("foo" "fob")))
        (should (equal (nucleo-completion--candidate-score
                        (car all))
                       100))
        (should (nucleo-completion-tests--high-score-face-p
                 (get-text-property 0 'face highlighted)))))))

(ert-deftest nucleo-completion-lazy-score-band-skips-exact-cache-for-high-score-test ()
  (let ((completion-ignore-case nil)
        (completion-lazy-hilit t)
        (nucleo-completion-highlight-score-bands t)
        (nucleo-completion-high-score-ratio 0.85)
        (nucleo-completion-max-highlighted-completions 0)
        (make-hash-table-calls 0)
        (original-make-hash-table (symbol-function 'make-hash-table)))
    (cl-letf (((symbol-function 'make-hash-table)
               (lambda (&rest args)
                 (setq make-hash-table-calls (1+ make-hash-table-calls))
                 (apply original-make-hash-table args)))
              ((symbol-function 'nucleo-completion--module-ready-p)
               (lambda () t))
              ((symbol-function 'nucleo-completion-candidates)
               (lambda (_needle _candidates _ignore-case _by-length
                                _alphabetically _limit
                                &optional return-all-scores)
                 (list '("foo")
                       nil
                       (when return-all-scores '(100))))))
      (let ((all (nucleo-completion-all-completions
                  "fo" '("foo" "bar"))))
        (should (= make-hash-table-calls 2))
        (setq make-hash-table-calls 0)
        (funcall completion-lazy-hilit-fn (copy-sequence (car all)))
        (should (= make-hash-table-calls 0))))))

(ert-deftest nucleo-completion-lazy-score-band-caches-exact-word-regexps-test ()
  (let ((completion-ignore-case nil)
        (completion-lazy-hilit t)
        (nucleo-completion-highlight-score-bands t)
        (nucleo-completion-max-highlighted-completions 0)
        (calls 0)
        (original-exact-word-regexps-1
         (symbol-function 'nucleo-completion--exact-word-regexps-1)))
    (cl-letf (((symbol-function 'nucleo-completion-candidates)
               (lambda (_needle _candidates _ignore-case _by-length
                                _alphabetically _limit
                                &optional return-all-scores)
                 (list '("foo-top" "bar-one" "bar-two")
                       nil
                       (when return-all-scores '(100 10 9)))))
              ((symbol-function 'nucleo-completion--exact-word-regexps-1)
               (lambda (needle)
                 (setq calls (1+ calls))
                 (funcall original-exact-word-regexps-1 needle))))
      (let ((all (nucleo-completion-all-completions
                  "foo" '("foo-top" "bar-one" "bar-two"))))
        (funcall completion-lazy-hilit-fn (cadr all))
        (funcall completion-lazy-hilit-fn (caddr all))
        (should (= calls 1))))))

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


(ert-deftest nucleo-completion-lazy-expansion-cache-is-per-result-test ()
  (let ((completion-lazy-hilit t)
        (completion-ignore-case nil)
        (nucleo-completion-tests--regexp-calls 0)
        (nucleo-completion-regexp-functions
         (list #'nucleo-completion-tests--nihon-regexp)))
    (cl-letf (((symbol-function 'nucleo-completion--module-ready-p) (lambda () nil)))
      (nucleo-completion-all-completions "nihon" '("日本語" "日本史" "nihon"))
      (let ((first completion-lazy-hilit-fn))
        (should (= nucleo-completion-tests--regexp-calls 1))
        (dolist (candidate '("日本語" "日本史" "nihon"))
          (funcall first candidate))
        (should (= nucleo-completion-tests--regexp-calls 1))
        (nucleo-completion-all-completions "nihon go" '("日本語" "nihon-go"))
        (let ((second completion-lazy-hilit-fn)
              (calls nucleo-completion-tests--regexp-calls))
          (should (> calls 1))
          (let ((nucleo-completion-regexp-functions nil)
                (nucleo-completion-regexp-minimum-term-length 100)
                (completion-ignore-case t))
            (with-temp-buffer
              (should (get-text-property 0 'face (funcall first "日本語")))
              (should (get-text-property 0 'face (funcall second "日本語")))
              (should-not (get-text-property 0 'face (funcall first "NIHON")))))
          (should (= nucleo-completion-tests--regexp-calls calls)))))))

(ert-deftest nucleo-completion-interrupted-lazy-expansion-cache-test ()
  (let ((completion-lazy-hilit t)
        (nucleo-completion-highlight-score-bands t)
        (nucleo-completion-tests--regexp-calls 0)
        (nucleo-completion-regexp-functions
         (list #'nucleo-completion-tests--nihon-regexp))
        (table '("日本語" "日本史" "nihon")))
    (cl-letf (((symbol-function 'nucleo-completion--module-ready-p) (lambda () nil)))
      (nucleo-completion-all-completions "nihon" table)
      (let ((old-callback completion-lazy-hilit-fn)
            (unread-command-events (list ?x)))
        (should (nucleo-completion-all-completions "nihon" table))
        (should-not (eq old-callback completion-lazy-hilit-fn)))
      (let ((calls nucleo-completion-tests--regexp-calls)
            (candidate (propertize "日本語" 'nucleo-completion-score 100)))
        (dolist (_ (number-sequence 1 3))
          (let ((result (funcall completion-lazy-hilit-fn candidate)))
            (should (get-text-property 0 'face result))
            (should-not (memq 'nucleo-completion-high-score-face
                              (ensure-list (get-text-property 0 'face result))))))
        (should (= calls nucleo-completion-tests--regexp-calls))))))

(ert-deftest nucleo-completion-native-language-case-and-pinyin-boundaries-test ()
  (skip-unless (and (fboundp 'nucleo-completion-migemo-regexp)
                    (fboundp 'nucleo-completion-pinyin-regexp)))
  (let ((nucleo-completion-languages '(japanese chinese))
        (nucleo-completion-regexp-functions nil))
    (dolist (needle '("FOO" "FOo"))
      (let ((completion-ignore-case nil))
        (should-not (member "foo" (nucleo-completion-tests--plain
                                   (nucleo-completion-all-completions needle '("foo"))))))
      (let ((completion-ignore-case t))
        (should (member "foo" (nucleo-completion-tests--plain
                               (nucleo-completion-all-completions needle '("foo")))))))
    (dolist (needle '("NIHON" "Nihon"))
      (should (member "日本" (nucleo-completion-tests--plain
                             (nucleo-completion-all-completions needle '("日本"))))))
    (dolist (case '(("lv" "绿") ("lü" "绿") ("LÜ" "绿") ("lu:" "绿")
                    ("nv" "女") ("nü" "女") ("NU:" "女")
                    ("xi'an" "西安") ("xian" "西安") ("xian" "先")))
      (let ((regexp (nucleo-completion-pinyin-regexp (car case))))
        (should (and (stringp regexp) (string-match-p regexp (cadr case))))))
    (let ((nucleo-completion-languages '(chinese)))
      (dolist (case '(("lü" "绿") ("lv" "绿") ("lu:" "绿")
                      ("nü" "女") ("nv" "女") ("nu:" "女")))
        (should (equal (nucleo-completion-tests--plain
                        (nucleo-completion-all-completions
                         (car case) (list (cadr case) "unrelated")))
                       (list (cadr case)))))
      (should (equal (nucleo-completion-tests--plain
                      (nucleo-completion-all-completions "xi'an" '("西安" "先")))
                     '("西安"))))
    (should-not (string-match-p (nucleo-completion-pinyin-regexp "xi'an") "先"))
    (dolist (needle '("'xian" "xian'" "xi''an"))
      (should (member (nucleo-completion-pinyin-regexp needle) '(nil ""))))))


(ert-deftest nucleo-completion-native-regexp-groups-integration-test ()
  "Native extras, including an empty list, bypass the Lisp scan."
  (dolist (extras '(nil ("日本語")))
    (let ((nucleo-completion--native-regexp-minimum-candidates 1)
          (nucleo-completion-languages '(japanese))
          (nucleo-completion-regexp-functions nil)
          (completion-ignore-case nil)
          (nucleo-completion-regexp-only-match-priority 'before))
      (cl-letf (((symbol-function 'nucleo-completion--module-ready-p) (lambda () t))
                ((symbol-function 'nucleo-completion--history-ranking) (lambda (&rest _) nil))
                ((symbol-function 'nucleo-completion--regexp-only-regexp-groups)
                 (lambda (_) '(("日本語"))))
                ((symbol-function 'nucleo-completion--regexp-only-candidates)
                 (lambda (&rest _) (error "Unexpected Lisp scan")))
                ((symbol-function 'nucleo-completion-candidates-with-regexp-groups)
                 (lambda (_needle _all _case _length _alpha _history _limit scores groups)
                   (should (equal groups '(("日本語"))))
                   (list '("nihon") '(("nihon" 10 (0))) (and scores '(10)) extras))))
        (let* ((result (nucleo-completion--legacy-module-completion-results
                        "" "nihon" '("nihon" "日本語") t 1 t))
               (bundle (nth 1 result)))
          (should (equal (nucleo-completion-tests--plain (car bundle))
                         (append extras '("nihon"))))
          (should (equal (nth 2 bundle) (if extras '(10 10) '(10)))))))))

(ert-deftest nucleo-completion-native-regexp-groups-unsupported-falls-back-test ()
  (let ((nucleo-completion--native-regexp-minimum-candidates 1)
        (nucleo-completion-languages '(japanese))
        (nucleo-completion-regexp-functions nil)
        (completion-ignore-case nil)
        (calls nil))
    (cl-letf (((symbol-function 'nucleo-completion--module-ready-p) (lambda () t))
              ((symbol-function 'nucleo-completion--history-ranking) (lambda (&rest _) nil))
              ((symbol-function 'nucleo-completion--regexp-only-regexp-groups) (lambda (_) '(("x"))))
              ((symbol-function 'nucleo-completion-candidates-with-regexp-groups)
               (lambda (&rest _) (push 'native calls) 'nucleo-completion-unsupported-regexp))
              ((symbol-function 'nucleo-completion-candidates)
               (lambda (&rest _) (push 'legacy calls) (list nil nil nil)))
              ((symbol-function 'nucleo-completion--regexp-only-candidates)
               (lambda (&rest _) (push 'scan calls) '("x"))))
      (should (equal (car (nucleo-completion--legacy-module-completion-results
                           "" "x" '("x") t 0 nil)) '("x")))
      (should (equal calls '(scan legacy native))))))

(ert-deftest nucleo-completion-native-regexp-groups-guards-test ()
  (dolist (settings '((nil nil nil) ((japanese) (identity) nil) ((japanese) nil t)))
    (let ((nucleo-completion--native-regexp-minimum-candidates 1)
          (nucleo-completion-languages (nth 0 settings))
          (nucleo-completion-regexp-functions (nth 1 settings))
          (completion-ignore-case (nth 2 settings)))
      (cl-letf (((symbol-function 'nucleo-completion--module-ready-p) (lambda () t))
                ((symbol-function 'nucleo-completion--history-ranking) (lambda (&rest _) nil))
                ((symbol-function 'nucleo-completion-candidates-with-regexp-groups)
                 (lambda (&rest _) (error "Unexpected native regexp call")))
                ((symbol-function 'nucleo-completion-candidates) (lambda (&rest _) (list nil nil nil)))
                ((symbol-function 'nucleo-completion--regexp-only-candidates) (lambda (&rest _) nil)))
        (nucleo-completion--legacy-module-completion-results "" "x" '("x") t 0 nil)))))

(ert-deftest nucleo-completion-native-regexp-groups-restores-extra-test ()
  (let* ((tofu (string #x3fff80))
         (first (propertize (concat "日本" tofu) 'source 1))
         (second (propertize (concat "日本" tofu tofu) 'source 2))
         (nucleo-completion-scrub-non-unicode-candidates t))
    (pcase-let* ((`(,cleaned . ,map)
                  (nucleo-completion--scrub-candidates (list first second) t))
                 (bundle (list nil nil nil (mapcar #'copy-sequence cleaned))))
      (nucleo-completion--restore-bundle-candidates bundle map)
      (should (eq (nth 0 (nth 3 bundle)) first))
      (should (eq (nth 1 (nth 3 bundle)) second)))))

(ert-deftest nucleo-completion-native-regexp-groups-scrub-falls-back-test ()
  (let* ((candidate (concat "日" (string #x3fff80) "本"))
         (nucleo-completion--native-regexp-minimum-candidates 1)
         (nucleo-completion-languages '(japanese))
         (nucleo-completion-regexp-functions nil)
         (nucleo-completion-scrub-non-unicode-candidates t)
         (completion-ignore-case nil))
    (cl-letf (((symbol-function 'nucleo-completion--module-ready-p) (lambda () t))
              ((symbol-function 'nucleo-completion--history-ranking) (lambda (&rest _) nil))
              ((symbol-function 'nucleo-completion--regexp-only-regexp-groups) (lambda (_) '(("日本"))))
              ((symbol-function 'nucleo-completion-candidates-with-regexp-groups)
               (lambda (&rest _) (error "Scrubbed regexp scan is unsafe")))
              ((symbol-function 'nucleo-completion-candidates) (lambda (&rest _) (list nil nil nil))))
      (should-not (car (nucleo-completion--legacy-module-completion-results
                        "" "nihon" (list candidate) t 0 nil))))))

(ert-deftest nucleo-completion-native-regexp-groups-interrupted-test ()
  (let ((nucleo-completion--native-regexp-minimum-candidates 1)
        (nucleo-completion-languages '(japanese))
        (nucleo-completion-regexp-functions nil)
        (completion-ignore-case nil))
    (cl-letf (((symbol-function 'nucleo-completion--module-ready-p) (lambda () t))
              ((symbol-function 'nucleo-completion--history-ranking) (lambda (&rest _) nil))
              ((symbol-function 'nucleo-completion--regexp-only-regexp-groups) (lambda (_) '(("x"))))
              ((symbol-function 'nucleo-completion-candidates-with-regexp-groups)
               (lambda (&rest _) (list nucleo-completion--interrupted-sentinel nil nil nil))))
      (should (eq (catch 'nucleo-completion-interrupted
                    (nucleo-completion--legacy-module-completion-results "" "x" '("x") t 0 nil))
                  nucleo-completion--interrupted-sentinel)))))

(ert-deftest nucleo-completion-native-regexp-groups-match-legacy-test ()
  "Built-in expansion produces the same bundles as the Lisp scan."
  (skip-unless (and (nucleo-completion--module-ready-p)
                    (fboundp 'nucleo-completion-candidates-with-regexp-groups)
                    (fboundp 'nucleo-completion-migemo-regexp)
                    (fboundp 'nucleo-completion-pinyin-regexp)
                    (fboundp 'nucleo-completion-hangul-regexp)))
  (let ((nucleo-completion-regexp-functions nil)
        (completion-ignore-case nil))
    (dolist (case '(((japanese) "nihon txt" ("日本.txt" "nihon.txt" "日本.org" "日本\ntxt"))
                    ((chinese) "zhongg" ("中国" "zhongg" "中\n国" "中國"))
                    ((korean) "ㄱㅅ" ("감사" "가수" "나무" "ㄱㅅ"))))
      (let ((nucleo-completion--native-regexp-minimum-candidates 1)
            (nucleo-completion-languages (nth 0 case))
            (needle (nth 1 case))
            (all (nth 2 case)))
        (dolist (priority '(before after non-ascii))
          (dolist (scores '(nil t))
            (let* ((nucleo-completion-regexp-only-match-priority priority)
                   (legacy (cl-letf (((symbol-function 'nucleo-completion-candidates-with-regexp-groups)
                                      (lambda (&rest _) 'nucleo-completion-unsupported-regexp)))
                             (nucleo-completion--legacy-module-completion-results "" needle all t 5 scores)))
                   (native (nucleo-completion--legacy-module-completion-results "" needle all t 5 scores)))
              (should (equal-including-properties legacy native)))))))))

(ert-deftest nucleo-completion-native-regexp-groups-small-list-test ()
  "Small lists avoid the cost of compiling native regexps."
  (let ((nucleo-completion-languages '(japanese))
        (nucleo-completion-regexp-functions nil)
        (nucleo-completion--native-regexp-minimum-candidates 3)
        (completion-ignore-case nil))
    (cl-letf (((symbol-function 'nucleo-completion--module-ready-p) (lambda () t))
              ((symbol-function 'nucleo-completion--history-ranking) (lambda (&rest _) nil))
              ((symbol-function 'nucleo-completion-candidates-with-regexp-groups)
               (lambda (&rest _) (error "Unexpected native regexp compilation")))
              ((symbol-function 'nucleo-completion-candidates)
               (lambda (&rest _) (list '("x") nil nil)))
              ((symbol-function 'nucleo-completion--regexp-only-candidates)
               (lambda (&rest _) '("日本"))))
      (should (equal (car (nucleo-completion--legacy-module-completion-results
                           "" "x" '("x" "日本") t 0 nil))
                     '("日本" "x"))))))

(ert-deftest nucleo-completion-native-regexp-groups-history-and-identity-test ()
  (skip-unless (fboundp 'nucleo-completion-candidates-with-regexp-groups))
  (let* ((first (propertize "日本" 'source 1))
         (second (propertize "日本" 'source 2))
         (candidates (list "a_b" first "a-b" second))
         (ranks '(1 nil 0 nil))
         (legacy (nucleo-completion-candidates-with-history
                  "ab" candidates nil t t ranks 2 t))
         (native (nucleo-completion-candidates-with-regexp-groups
                  "ab" candidates nil t t ranks 2 t '(("日本")))))
    (should (equal-including-properties legacy (cl-subseq native 0 3)))
    (should (eq first (car (nth 3 native))))
    (should (eq second (cadr (nth 3 native))))))

(ert-deftest nucleo-completion-native-regexp-groups-extra-build-interruption-test ()
  (skip-unless (fboundp 'nucleo-completion-candidates-with-regexp-groups))
  (let ((calls 0)
        (candidates (make-list 600 "日本")))
    (cl-letf (((symbol-function 'input-pending-p)
               (lambda (&optional _check-timers)
                 (setq calls (1+ calls))
                 (>= calls 10))))
      (should (nucleo-completion--bundle-interrupted-p
               (nucleo-completion-candidates-with-regexp-groups
                "no-match" candidates nil nil nil nil 0 nil '(("日本")))))
      (should (= calls 10)))))


(defun nucleo-completion-tests--equal-with-properties (left right)
  "Compare nested LEFT and RIGHT, including string text properties."
  (cond ((and (stringp left) (stringp right))
         (equal-including-properties left right))
        ((and (consp left) (consp right))
         (and (nucleo-completion-tests--equal-with-properties (car left) (car right))
              (nucleo-completion-tests--equal-with-properties (cdr left) (cdr right))))
        (t (equal left right))))

(ert-deftest nucleo-completion-native-options-match-legacy-test ()
  "Optimized history and regexp results retain the legacy bundle semantics."
  (skip-unless (and (fboundp 'nucleo-completion-candidates-with-options)
                    (fboundp 'nucleo-completion-migemo-regexp)))
  (let* ((first (propertize "日本" 'source 1))
         (second (propertize "日本" 'source 2))
         (all (list "nihon" first "nihon.txt" second "other"))
         (nucleo-completion--native-regexp-minimum-candidates 1)
         (nucleo-completion-languages '(japanese))
         (nucleo-completion-regexp-functions nil)
         (nucleo-completion-sort-ties-by-history t)
         (minibuffer-history-variable 'nucleo-completion-tests-history)
         (nucleo-completion-tests-history '("/tmp/nihon.txt" "/tmp/nihon"))
         (completion-ignore-case nil))
    (cl-letf (((symbol-function 'minibufferp) (lambda (&rest _) t)))
      (dolist (priority '(before after non-ascii))
        (dolist (bands '(nil t))
          (dolist (scores '(nil t))
            (dolist (limit '(0 1 5))
              (let* ((nucleo-completion-regexp-only-match-priority priority)
                     (nucleo-completion-highlight-score-bands bands)
                     (legacy (nucleo-completion--legacy-module-completion-results
                              "/tmp/" "nihon" all t limit scores))
                     (native (nucleo-completion--module-completion-results
                              "/tmp/" "nihon" all t limit scores)))
                (should (nucleo-completion-tests--equal-with-properties legacy native))
                (unless bands
                  (should (memq first (car native)))
                  (should (memq second (car native))))))))))))

(ert-deftest nucleo-completion-native-options-no-fuzzy-matches-test ()
  (skip-unless (and (fboundp 'nucleo-completion-candidates-with-options)
                    (fboundp 'nucleo-completion-migemo-regexp)))
  (let ((nucleo-completion--native-regexp-minimum-candidates 1)
        (nucleo-completion-languages '(japanese))
        (nucleo-completion-regexp-functions nil)
        (completion-ignore-case nil))
    (dolist (all (list (list (propertize "日本" 'source 1) "日本語")
                       '("unrelated" "other")))
      (dolist (priority '(before after non-ascii))
        (dolist (bands '(nil t))
          (dolist (scores '(nil t))
            (dolist (limit '(0 2))
              (let ((nucleo-completion-regexp-only-match-priority priority)
                    (nucleo-completion-highlight-score-bands bands))
                (should
                 (nucleo-completion-tests--equal-with-properties
                  (nucleo-completion--legacy-module-completion-results
                   "" "nihon" all t limit scores)
                  (nucleo-completion--module-completion-results
                   "" "nihon" all t limit scores)))))))))))

(ert-deftest nucleo-completion-native-options-corfu-history-test ()
  (skip-unless (fboundp 'nucleo-completion-candidates-with-options))
  (let ((nucleo-completion-sort-ties-by-history t)
        (corfu-history-mode t)
        (corfu-history '("a-b" "a_b"))
        (all '("a_b" "a-b" "ab" "AB")))
    (cl-letf (((symbol-function 'minibufferp) (lambda (&rest _) nil)))
      (dolist (completion-ignore-case '(nil t))
        (dolist (by-length '(nil t))
          (dolist (alphabetically '(nil t))
            (let ((nucleo-completion-sort-ties-by-length by-length)
                  (nucleo-completion-sort-ties-alphabetically alphabetically))
              (should (nucleo-completion-tests--equal-with-properties
                       (nucleo-completion--legacy-module-completion-results
                        "" "ab" all nil 2 t)
                       (nucleo-completion--module-completion-results
                        "" "ab" all nil 2 t))))))))))

(ert-deftest nucleo-completion-native-options-avoids-lisp-ranking-and-merge-test ()
  (skip-unless (fboundp 'nucleo-completion-candidates-with-options))
  (let ((nucleo-completion-sort-ties-by-history t)
        (corfu-history-mode t)
        (corfu-history '("ab"))
        (nucleo-completion--native-regexp-minimum-candidates 1)
        (nucleo-completion-languages '(japanese))
        (nucleo-completion-regexp-functions nil)
        (completion-ignore-case nil))
    (cl-letf (((symbol-function 'minibufferp) (lambda (&rest _) nil))
              ((symbol-function 'nucleo-completion--regexp-only-regexp-groups)
               (lambda (_) '(("日本"))))
              ((symbol-function 'nucleo-completion--history-ranking)
               (lambda (&rest _) (error "Unexpected candidate history scan")))
              ((symbol-function 'nucleo-completion--merge-regexp-only-matches)
               (lambda (&rest _) (error "Unexpected Lisp merge"))))
      (should (equal (car (nucleo-completion--module-completion-results
                           "" "ab" '("ab" "日本") t 1 nil))
                     '("日本" "ab"))))))

(ert-deftest nucleo-completion-native-options-scrubbing-falls-back-test ()
  (let ((nucleo-completion-scrub-non-unicode-candidates t)
        (nucleo-completion-sort-ties-by-history t))
    (cl-letf (((symbol-function 'nucleo-completion--module-ready-p) (lambda () t))
              ((symbol-function 'nucleo-completion-candidates-with-options)
               (lambda (&rest _) (error "Explicit scrubbing must use legacy path"))))
      (should (eq (nucleo-completion--native-completion-results
                   "" "a" '("a" "ab") nil 0 nil)
                  'nucleo-completion-unsupported-regexp)))))

(ert-deftest nucleo-completion-native-options-non-unicode-history-falls-back-test ()
  (let ((rank-table (make-hash-table :test #'equal)))
    (puthash (string #x3fff80) 0 rank-table)
    (cl-letf (((symbol-function 'nucleo-completion--module-ready-p) (lambda () t))
              ((symbol-function 'nucleo-completion--history-rank-table)
               (lambda (_) rank-table))
              ((symbol-function 'nucleo-completion-candidates-with-options)
               (lambda (&rest _) (error "Non-Unicode history must use legacy path"))))
      (should (eq (nucleo-completion--native-completion-results
                   "" "a" '("a" "ab") nil 0 nil)
                  'nucleo-completion-unsupported-regexp)))))

(ert-deftest nucleo-completion-native-options-interrupted-test ()
  (let ((rank-table (make-hash-table :test #'equal)))
    (puthash "ab" 0 rank-table)
    (cl-letf (((symbol-function 'nucleo-completion--module-ready-p) (lambda () t))
              ((symbol-function 'nucleo-completion--history-rank-table)
               (lambda (_) rank-table))
              ((symbol-function 'nucleo-completion-candidates-with-options)
               (lambda (&rest _)
                 (list nucleo-completion--interrupted-sentinel nil nil))))
      (should (eq (catch 'nucleo-completion-interrupted
                    (nucleo-completion--module-completion-results
                     "" "a" '("a" "ab") nil 0 nil))
                  nucleo-completion--interrupted-sentinel)))))

(ert-deftest nucleo-completion-native-options-unicode-error-falls-back-test ()
  (let ((rank-table (make-hash-table :test #'equal))
        (candidate (concat "a" (string #x3fff80))))
    (puthash "a" 0 rank-table)
    (cl-letf (((symbol-function 'nucleo-completion--module-ready-p) (lambda () t))
              ((symbol-function 'nucleo-completion--history-rank-table)
               (lambda (_) rank-table))
              ((symbol-function 'nucleo-completion-candidates-with-options)
               (lambda (&rest _) (signal 'wrong-type-argument
                                         (list 'unicode-string-p candidate))))
              ((symbol-function 'nucleo-completion--legacy-module-completion-results)
               (lambda (&rest _) 'legacy-result)))
      (should (eq (nucleo-completion--module-completion-results
                   "" "a" (list candidate "ab") nil 0 nil)
                  'legacy-result)))))

(ert-deftest nucleo-completion-native-options-unsupported-regexp-retries-test ()
  (let ((nucleo-completion--native-regexp-minimum-candidates 1)
        (nucleo-completion-languages '(japanese))
        (nucleo-completion-regexp-functions nil)
        (completion-ignore-case nil)
        (calls nil))
    (cl-letf (((symbol-function 'nucleo-completion--module-ready-p) (lambda () t))
              ((symbol-function 'nucleo-completion--history-rank-table) (lambda (_) nil))
              ((symbol-function 'nucleo-completion--regexp-only-regexp-groups)
               (lambda (_) '(("日本"))))
              ((symbol-function 'nucleo-completion-candidates-with-options)
               (lambda (&rest args)
                 (push (nth 8 args) calls)
                 (if (nth 8 args) 'nucleo-completion-unsupported-regexp
                   (list '("ab") nil nil))))
              ((symbol-function 'nucleo-completion--regexp-only-candidates)
               (lambda (&rest _) '("日本")))
              ((symbol-function 'nucleo-completion--legacy-module-completion-results)
               (lambda (&rest _) (error "Should retry the new fuzzy API"))))
      (should (equal (car (nucleo-completion--module-completion-results
                           "" "ab" '("ab" "日本") t 0 nil))
                     '("日本" "ab")))
      (should (equal (nreverse calls) '((("日本")) nil))))))

(ert-deftest nucleo-completion-native-options-history-decode-interruption-test ()
  (skip-unless (fboundp 'nucleo-completion-candidates-with-options))
  (let ((calls 0)
        (history (cl-loop for i below 300 collect (cons (format "h%d" i) i))))
    (cl-letf (((symbol-function 'input-pending-p)
               (lambda (&optional _) (>= (cl-incf calls) 2))))
      ;; The first check is at entry; the second is while decoding history.
      (should (equal (nucleo-completion-candidates-with-options
                      "a" '("a") nil nil nil history 0 nil nil 'before nil)
                     (list nucleo-completion--interrupted-sentinel nil nil)))
      (should (= calls 2)))))

(ert-deftest nucleo-completion-native-options-extra-assembly-interruption-test ()
  (skip-unless (fboundp 'nucleo-completion-candidates-with-options))
  (let ((calls 0)
        (candidates (make-list 600 "日本")))
    (cl-letf (((symbol-function 'input-pending-p)
               (lambda (&optional _) (>= (cl-incf calls) 8))))
      ;; Checks 6 and 7 partition extras; check 8 builds their result list.
      (should (equal (nucleo-completion-candidates-with-options
                      "no-match" candidates nil nil nil nil 0 nil
                      '(("日本")) 'before nil)
                     (list nucleo-completion--interrupted-sentinel nil nil)))
      (should (= calls 8)))))

(ert-deftest nucleo-completion-native-options-non-unicode-real-fallback-test ()
  (skip-unless (fboundp 'nucleo-completion-candidates-with-options))
  (let* ((first (concat "same" (string #x200004)))
         (second (concat "same" (string #x200005)))
         (all (list first second "same"))
         (nucleo-completion-sort-ties-by-history t)
         (minibuffer-history-variable 'nucleo-completion-tests-history)
         (nucleo-completion-scrub-non-unicode-candidates nil)
         (nucleo-completion--force-scrub-non-unicode-candidates nil)
         (completion-ignore-case nil))
    (cl-letf (((symbol-function 'minibufferp) (lambda (&rest _) t)))
      ;; The first case rejects history keys; the second reaches the module's
      ;; Unicode encoder before retrying through the scrub-capable legacy path.
      (dolist (nucleo-completion-tests-history
               (list (list second first) '("same")))
        (should (nucleo-completion-tests--equal-with-properties
                 (nucleo-completion--legacy-module-completion-results
                  "" "same" all nil 2 nil)
                 (nucleo-completion--module-completion-results
                  "" "same" all nil 2 nil)))))))

(ert-deftest nucleo-completion-native-options-unavailable-falls-back-test ()
  (let ((original-fboundp (symbol-function 'fboundp)))
    (cl-letf (((symbol-function 'nucleo-completion--module-ready-p) (lambda () t))
              ((symbol-function 'fboundp)
               (lambda (symbol)
                 (and (not (eq symbol 'nucleo-completion-candidates-with-options))
                      (funcall original-fboundp symbol))))
              ((symbol-function 'nucleo-completion--legacy-module-completion-results)
               (lambda (&rest _) 'legacy-result)))
      (should (eq (nucleo-completion--module-completion-results
                   "" "a" '("a" "ab") nil 0 nil)
                  'legacy-result)))))

;;; nucleo-completion-tests.el ends here
