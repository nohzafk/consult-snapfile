;;; test-consult-snapfile-integration.el --- Integration test for consult-snapfile -*- lexical-binding: t -*-

;; Run with: emacs --batch -l emacs/tests/integration.el
;; Requires: Server running on port 9876

;;; Code:

;; Add load paths
(let ((test-dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name ".." test-dir)))


;; Load required packages
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(require 'consult)
(require 'consult-snapfile-server)
(require 'consult-snapfile)

(defvar test-passed 0)
(defvar test-failed 0)
(defvar test-results nil)
(defvar test-complete nil)
(defvar test-error nil)

(defun test-assert (name condition)
  "Assert CONDITION is true for test NAME."
  (if condition
      (progn
        (princ (format "  ✓ %s\n" name))
        (setq test-passed (1+ test-passed)))
    (princ (format "  ✗ %s\n" name))
    (setq test-failed (1+ test-failed))))

(defun test-message-handler (type msg)
  "Handle server messages for testing."
  (pcase type
    ('results
     (let ((items (plist-get msg :items)))
       ;; Convert vector to list if needed
       (when (vectorp items)
         (setq items (append items nil)))
       (setq test-results (append test-results items))))
    ('complete
     (setq test-complete t))
    ('error
     (setq test-error (plist-get msg :message)))))

(princ "\n=== consult-snapfile Integration Tests ===\n")
(princ "(Requires server running on port 9876)\n\n")

;; Test 1: Connect to server
(princ "--- Test 1: Server Connection ---\n")
(condition-case err
    (progn
      (consult-snapfile--ensure-connected)
      ;; Wait for connection
      (let ((timeout 50))  ; 5 seconds
        (while (and (> timeout 0) (not (consult-snapfile-connected-p)))
          (sleep-for 0.1)
          (setq timeout (1- timeout))))
      (test-assert "Connected to server" (consult-snapfile-connected-p)))
  (error
   (test-assert "Connected to server" nil)
   (princ (format "  Error: %s\n" (error-message-string err)))
   (kill-emacs 1)))

;; Test 2: Search for files
(princ "\n--- Test 2: File Search ---\n")
(setq test-results nil)
(setq test-complete nil)
(setq test-error nil)

(consult-snapfile-add-message-handler #'test-message-handler)

;; Send search request
(let ((request-id (consult-snapfile--uuid)))
  (consult-snapfile--send
   `(:type "search"
     :id ,request-id
     :mode "files"
     :query "rs"
     :cwd ,(expand-file-name ".")
     :options (:max_results 10)))

  ;; Wait for results (max 5 seconds)
  (let ((timeout 50))
    (while (and (> timeout 0) (not test-complete) (not test-error))
      (sleep-for 0.1)
      (setq timeout (1- timeout))))

  (test-assert "No error" (null test-error))
  (test-assert "Search completed" test-complete)
  (test-assert "Got results" (> (length test-results) 0))

  (when test-results
    (princ (format "  Found %d files matching 'rs'\n" (length test-results)))
    (princ "  Sample results:\n")
    (dolist (item (seq-take test-results 3))
      (princ (format "    - %s\n" (plist-get item :text))))))

(when test-error
  (princ (format "  Error: %s\n" test-error)))

;; Test 3: Empty query (should return all files)
(princ "\n--- Test 3: Empty Query (All Files) ---\n")
(setq test-results nil)
(setq test-complete nil)
(setq test-error nil)

(let ((request-id (consult-snapfile--uuid)))
  (consult-snapfile--send
   `(:type "search"
     :id ,request-id
     :mode "files"
     :query ""
     :cwd ,(expand-file-name ".")
     :options (:max_results 5)))

  ;; Wait for results
  (let ((timeout 50))
    (while (and (> timeout 0) (not test-complete) (not test-error))
      (sleep-for 0.1)
      (setq timeout (1- timeout))))

  (test-assert "Empty query completed" test-complete)
  (test-assert "Empty query got results" (> (length test-results) 0))
  (princ (format "  Got %d files (max 5)\n" (length test-results))))

;; Test 4: Fuzzy matching
(princ "\n--- Test 4: Fuzzy Matching ---\n")
(setq test-results nil)
(setq test-complete nil)
(setq test-error nil)

(let ((request-id (consult-snapfile--uuid)))
  (consult-snapfile--send
   `(:type "search"
     :id ,request-id
     :mode "files"
     :query "emacs"  ; should match emacs/ directory files
     :cwd ,(expand-file-name ".")
     :options (:max_results 5)))

  (let ((timeout 50))
    (while (and (> timeout 0) (not test-complete) (not test-error))
      (sleep-for 0.1)
      (setq timeout (1- timeout))))

  (test-assert "Fuzzy search completed" test-complete)
  ;; Debug: show what we got
  (princ (format "  Got %d results:\n" (length test-results)))
  (dolist (item test-results)
    (princ (format "    - %s (score: %s)\n"
                   (plist-get item :text)
                   (plist-get item :score))))
  (let ((found-emacs-file (seq-find (lambda (item)
                                      (string-match-p "emacs/" (plist-get item :text)))
                                    test-results)))
    (test-assert "Found emacs/ file with query 'emacs'" found-emacs-file)
    (when found-emacs-file
      (princ (format "  Best match: %s (score: %s)\n"
                     (plist-get found-emacs-file :text)
                     (plist-get found-emacs-file :score))))))

;; Cleanup
(consult-snapfile-remove-message-handler #'test-message-handler)
(consult-snapfile-server-stop)

;; Summary
(princ (format "\n=== Results: %d passed, %d failed ===\n" test-passed test-failed))

(kill-emacs (if (> test-failed 0) 1 0))

;;; test-consult-snapfile-integration.el ends here
