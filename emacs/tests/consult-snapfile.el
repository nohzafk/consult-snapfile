;;; test-consult-snapfile.el --- Test consult-snapfile integration -*- lexical-binding: t -*-

;; Run with: emacs --batch -l emacs/tests/consult-snapfile.el

;;; Code:

;; Add load paths
(let ((test-dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name ".." test-dir)))

;; Install websocket if needed via package.el
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(unless (package-installed-p 'websocket)
  (princ "Installing websocket package...\n")
  (package-refresh-contents)
  (package-install 'websocket)
  (princ "Websocket installed.\n"))

(defvar test-passed 0)
(defvar test-failed 0)

(defun test-assert (name condition)
  "Assert CONDITION is true for test NAME."
  (if condition
      (progn
        (princ (format "  ✓ %s\n" name))
        (setq test-passed (1+ test-passed)))
    (princ (format "  ✗ %s\n" name))
    (setq test-failed (1+ test-failed))))

(princ "\n=== consult-snapfile Tests ===\n\n")

;; Test 1: Load consult-snapfile
(princ "--- Test 1: Load consult-snapfile ---\n")
(condition-case err
    (progn
      (require 'consult-snapfile)
      (test-assert "consult-snapfile loaded" t))
  (error
   (test-assert "consult-snapfile loaded" nil)
   (princ (format "  Error: %s\n" (error-message-string err)))))

;; Test 2: Functions defined
(princ "\n--- Test 2: Functions Defined ---\n")
(test-assert "consult-snapfile command exists" (fboundp 'consult-snapfile))
(test-assert "consult-snapfile-server-start exists" (fboundp 'consult-snapfile-server-start))
(test-assert "consult-snapfile-server-stop exists" (fboundp 'consult-snapfile-server-stop))
(test-assert "consult-snapfile-connect exists" (fboundp 'consult-snapfile-connect))

;; Test 3: Customization variables
(princ "\n--- Test 3: Customization Variables ---\n")
(test-assert "consult-snapfile-max-results defined" (boundp 'consult-snapfile-max-results))
(test-assert "default max-results is 100" (eq consult-snapfile-max-results 100))

;; Test 4: Internal functions
(princ "\n--- Test 4: Internal Functions ---\n")
(test-assert "message handler exists" (fboundp 'consult-snapfile--message-handler))
(test-assert "search function exists" (fboundp 'consult-snapfile--search))
(test-assert "async source exists" (fboundp 'consult-snapfile--async-source))

;; Test 5: Consult async source compatibility
(princ "\n--- Test 5: Consult Async Source ---\n")
(require 'consult)

;; Check our async source has the correct arity for consult
;; Consult detects async sources by checking (func-arity fun) == (1 . 1)
(let ((arity (func-arity #'consult-snapfile--async-source)))
  (test-assert "async source has arity (1 . 1)" (equal arity '(1 . 1)))
  (princ (format "  Async source arity: %s\n" arity)))

;; Check consult will recognize it as async
(test-assert "consult detects async source"
             (and (functionp #'consult-snapfile--async-source)
                  (equal (func-arity #'consult-snapfile--async-source) '(1 . 1))))

;; Summary
(princ (format "\n=== Results: %d passed, %d failed ===\n" test-passed test-failed))

(kill-emacs (if (> test-failed 0) 1 0))

;;; test-consult-snapfile.el ends here
