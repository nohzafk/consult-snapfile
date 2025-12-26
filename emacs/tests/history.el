;;; test-history.el --- Test frecency/history module -*- lexical-binding: t -*-

;; Run with: emacs --batch -l emacs/tests/history.el

;;; Code:

(let ((test-dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name ".." test-dir)))

(require 'consult-snapfile-history)

(defvar test-passed 0)
(defvar test-failed 0)

(defun test-assert (name expected actual)
  "Assert EXPECTED equals ACTUAL for test NAME."
  (if (equal expected actual)
      (progn
        (princ (format "✓ %s\n" name))
        (setq test-passed (1+ test-passed)))
    (princ (format "✗ %s\n  Expected: %S\n  Actual: %S\n" name expected actual))
    (setq test-failed (1+ test-failed))))

(defun test-assert-range (name min max actual)
  "Assert ACTUAL is between MIN and MAX for test NAME."
  (if (and (>= actual min) (<= actual max))
      (progn
        (princ (format "✓ %s (value: %d)\n" name actual))
        (setq test-passed (1+ test-passed)))
    (princ (format "✗ %s\n  Expected: %d-%d\n  Actual: %d\n" name min max actual))
    (setq test-failed (1+ test-failed))))

(princ "\n=== consult-snapfile Frecency Tests ===\n\n")

;; Use temp file for testing
(setq consult-snapfile-history-file (make-temp-file "consult-snapfile-history-test" nil ".el"))
(setq consult-snapfile-history--data (make-hash-table :test 'equal))
(setq consult-snapfile-history--loaded t)

;; Test 1: Initial boost is 0
(princ "--- Test 1: Initial state ---\n")
(test-assert "Unknown item has 0 boost"
             0
             (consult-snapfile-history-get-boost "nonexistent.txt" "files"))

;; Test 2: Record and boost
(princ "\n--- Test 2: Record selections ---\n")
(consult-snapfile-history-record "main.gleam" "files")
(consult-snapfile-history-record "main.gleam" "files")
(consult-snapfile-history-record "main.gleam" "files")
(consult-snapfile-history-record "router.gleam" "files")

;; main.gleam: 3 selections * 10 + 50 recency = 80
(test-assert-range "main.gleam boost (3 selections)"
                   75 85
                   (consult-snapfile-history-get-boost "main.gleam" "files"))

;; router.gleam: 1 selection * 10 + 50 recency = 60
(test-assert-range "router.gleam boost (1 selection)"
                   55 65
                   (consult-snapfile-history-get-boost "router.gleam" "files"))

;; Test 3: Mode isolation
(princ "\n--- Test 3: Mode isolation ---\n")
(test-assert "main.gleam in 'grep' mode has 0 boost"
             0
             (consult-snapfile-history-get-boost "main.gleam" "grep"))

(consult-snapfile-history-record "main.gleam" "grep")
(test-assert-range "main.gleam in 'grep' after 1 selection"
                   55 65
                   (consult-snapfile-history-get-boost "main.gleam" "grep"))

;; Test 4: Boost results list
(princ "\n--- Test 4: Boost results list ---\n")
(let* ((results '((:text "unknown.gleam" :score 100)
                  (:text "router.gleam" :score 100)
                  (:text "main.gleam" :score 100)))
       (boosted (consult-snapfile-history-boost-results results "files"))
       (first (car boosted))
       (second (cadr boosted))
       (third (caddr boosted)))

  ;; main.gleam should be first (highest boost)
  (test-assert "main.gleam is first after boost"
               "main.gleam"
               (plist-get first :text))

  ;; router.gleam should be second
  (test-assert "router.gleam is second after boost"
               "router.gleam"
               (plist-get second :text))

  ;; unknown.gleam should be last (no boost)
  (test-assert "unknown.gleam is last after boost"
               "unknown.gleam"
               (plist-get third :text))

  ;; Check scores are boosted correctly
  (test-assert-range "main.gleam boosted score"
                     175 185
                     (plist-get first :score)))

;; Test 5: Persistence
(princ "\n--- Test 5: Persistence ---\n")
(consult-snapfile-history-save)
(test-assert "History file created"
             t
             (file-exists-p consult-snapfile-history-file))

;; Reload and verify
(setq consult-snapfile-history--data (make-hash-table :test 'equal))
(setq consult-snapfile-history--loaded nil)
(consult-snapfile-history--ensure-loaded)
(test-assert-range "main.gleam boost after reload"
                   75 85
                   (consult-snapfile-history-get-boost "main.gleam" "files"))

;; Cleanup
(delete-file consult-snapfile-history-file)

;; Summary
(princ (format "\n=== Results: %d passed, %d failed ===\n"
               test-passed test-failed))

(kill-emacs (if (> test-failed 0) 1 0))

;;; test-history.el ends here
