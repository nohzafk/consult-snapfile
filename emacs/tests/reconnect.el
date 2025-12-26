;;; test-reconnect.el --- Test consult-snapfile reconnection logic -*- lexical-binding: t -*-

;; Run with: emacs --batch -l emacs/tests/reconnect.el

;;; Code:

(let ((test-dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name ".." test-dir)))

(require 'consult-snapfile-server)

(defvar test-passed 0)
(defvar test-failed 0)

(defun test-assert (name condition)
  "Assert CONDITION is true for test NAME."
  (if condition
      (progn
        (princ (format "  PASS %s\n" name))
        (setq test-passed (1+ test-passed)))
    (princ (format "  FAIL %s\n" name))
    (setq test-failed (1+ test-failed))))

(princ "\n=== consult-snapfile Reconnect Tests ===\n\n")

;; Test 1: Exponential backoff delay calculation
(princ "--- Test 1: Exponential Backoff Delay ---\n")
(let ((consult-snapfile-reconnect-base-delay 1.0)
      (consult-snapfile-reconnect-max-delay 30.0))
  (setq consult-snapfile--reconnect-attempts 0)
  (test-assert "Attempt 0: delay = 1s" (= (consult-snapfile--reconnect-delay) 1.0))
  (setq consult-snapfile--reconnect-attempts 1)
  (test-assert "Attempt 1: delay = 2s" (= (consult-snapfile--reconnect-delay) 2.0))
  (setq consult-snapfile--reconnect-attempts 2)
  (test-assert "Attempt 2: delay = 4s" (= (consult-snapfile--reconnect-delay) 4.0))
  (setq consult-snapfile--reconnect-attempts 3)
  (test-assert "Attempt 3: delay = 8s" (= (consult-snapfile--reconnect-delay) 8.0))
  (setq consult-snapfile--reconnect-attempts 4)
  (test-assert "Attempt 4: delay = 16s" (= (consult-snapfile--reconnect-delay) 16.0))
  (setq consult-snapfile--reconnect-attempts 5)
  (test-assert "Attempt 5: delay capped at 30s" (= (consult-snapfile--reconnect-delay) 30.0))
  (setq consult-snapfile--reconnect-attempts 10)
  (test-assert "Attempt 10: delay still capped at 30s" (= (consult-snapfile--reconnect-delay) 30.0)))

;; Test 2: Reset reconnect state
(princ "\n--- Test 2: Reset Reconnect State ---\n")
(setq consult-snapfile--reconnect-attempts 5)
(setq consult-snapfile--intentional-disconnect t)
(consult-snapfile--reset-reconnect-state)
(test-assert "Attempts reset to 0" (= consult-snapfile--reconnect-attempts 0))
(test-assert "Intentional disconnect reset" (null consult-snapfile--intentional-disconnect))

;; Test 3: Connection state tracking
(princ "\n--- Test 3: Connection State ---\n")
(setq consult-snapfile--connection-state 'disconnected)
(test-assert "Initial state is disconnected" (eq (consult-snapfile-connection-state) 'disconnected))
(test-assert "Not connected" (not (consult-snapfile-connected-p)))
(setq consult-snapfile--connection-state 'connecting)
(test-assert "State can be connecting" (eq (consult-snapfile-connection-state) 'connecting))
(test-assert "Connecting is not connected" (not (consult-snapfile-connected-p)))
(setq consult-snapfile--connection-state 'connected)
(test-assert "State can be connected" (eq (consult-snapfile-connection-state) 'connected))
(test-assert "Connected is connected" (consult-snapfile-connected-p))

;; Test 4: Clear pending requests
(princ "\n--- Test 4: Clear Pending Requests ---\n")
(clrhash consult-snapfile--request-handlers)
(puthash "req-1" (lambda () nil) consult-snapfile--request-handlers)
(puthash "req-2" (lambda () nil) consult-snapfile--request-handlers)
(puthash "req-3" (lambda () nil) consult-snapfile--request-handlers)
(test-assert "Has 3 pending requests" (= (hash-table-count consult-snapfile--request-handlers) 3))
(consult-snapfile--clear-pending-requests)
(test-assert "Requests cleared" (= (hash-table-count consult-snapfile--request-handlers) 0))

;; Test 5: Cancel reconnect timer
(princ "\n--- Test 5: Cancel Reconnect ---\n")
(setq consult-snapfile--reconnect-timer (run-with-timer 999 nil #'ignore))
(test-assert "Timer exists" (timerp consult-snapfile--reconnect-timer))
(consult-snapfile--cancel-reconnect)
(test-assert "Timer canceled" (null consult-snapfile--reconnect-timer))

;; Test 6: Schedule reconnect respects max attempts
(princ "\n--- Test 6: Max Reconnect Attempts ---\n")
(setq consult-snapfile--reconnect-attempts 0)
(setq consult-snapfile-reconnect-max-attempts 3)
(setq consult-snapfile--connection-state 'connected)
;; Simulate hitting max attempts
(setq consult-snapfile--reconnect-attempts 3)
(consult-snapfile--schedule-reconnect)
(test-assert "No timer when max reached" (null consult-snapfile--reconnect-timer))
(test-assert "State set to disconnected" (eq consult-snapfile--connection-state 'disconnected))

;; Test 7: Schedule reconnect creates timer when under max
(princ "\n--- Test 7: Schedule Reconnect Creates Timer ---\n")
(setq consult-snapfile--reconnect-attempts 0)
(setq consult-snapfile-reconnect-max-attempts 5)
(consult-snapfile--schedule-reconnect)
(test-assert "Timer created" (timerp consult-snapfile--reconnect-timer))
(test-assert "State is connecting" (eq consult-snapfile--connection-state 'connecting))
(consult-snapfile--cancel-reconnect)

;; Test 8: Intentional disconnect flag
(princ "\n--- Test 8: Intentional Disconnect ---\n")
(setq consult-snapfile--intentional-disconnect nil)
(setq consult-snapfile--connection nil)
(setq consult-snapfile--server-process nil)
(consult-snapfile-server-stop)
(test-assert "Stop sets intentional disconnect" consult-snapfile--intentional-disconnect)
(test-assert "Stop sets state to disconnected" (eq consult-snapfile--connection-state 'disconnected))

;; Test 9: Custom backoff settings
(princ "\n--- Test 9: Custom Backoff Settings ---\n")
(let ((consult-snapfile-reconnect-base-delay 2.0)
      (consult-snapfile-reconnect-max-delay 10.0))
  (setq consult-snapfile--reconnect-attempts 0)
  (test-assert "Custom base delay" (= (consult-snapfile--reconnect-delay) 2.0))
  (setq consult-snapfile--reconnect-attempts 1)
  (test-assert "Custom delay doubles" (= (consult-snapfile--reconnect-delay) 4.0))
  (setq consult-snapfile--reconnect-attempts 3)
  (test-assert "Custom max delay" (= (consult-snapfile--reconnect-delay) 10.0)))

;; Test 10: Request lifecycle - register and complete
(princ "\n--- Test 10: Request Lifecycle ---\n")
(clrhash consult-snapfile--request-timestamps)
(clrhash consult-snapfile--request-handlers)
(consult-snapfile--register-request "test-req-1")
(test-assert "Request registered" (gethash "test-req-1" consult-snapfile--request-timestamps))
(test-assert "Timestamp is recent" (< (- (float-time) (gethash "test-req-1" consult-snapfile--request-timestamps)) 1.0))

(consult-snapfile--complete-request "test-req-1")
(test-assert "Request removed from timestamps" (null (gethash "test-req-1" consult-snapfile--request-timestamps)))
(test-assert "Request removed from handlers" (null (gethash "test-req-1" consult-snapfile--request-handlers)))

;; Test 11: Stale request detection
(princ "\n--- Test 11: Stale Request Detection ---\n")
(clrhash consult-snapfile--request-timestamps)
(clrhash consult-snapfile--request-handlers)
;; Create a request with old timestamp (simulating stale request)
(puthash "stale-req" (- (float-time) 60) consult-snapfile--request-timestamps)  ;; 60 seconds ago
(puthash "fresh-req" (float-time) consult-snapfile--request-timestamps)  ;; now
(let ((consult-snapfile-request-timeout 30.0))  ;; 30 second timeout
  (consult-snapfile--check-stale-requests))
(test-assert "Stale request removed" (null (gethash "stale-req" consult-snapfile--request-timestamps)))
(test-assert "Fresh request kept" (gethash "fresh-req" consult-snapfile--request-timestamps))
;; Cleanup
(clrhash consult-snapfile--request-timestamps)

;; Test 12: Stale check timer
(princ "\n--- Test 12: Stale Check Timer ---\n")
(consult-snapfile--stop-stale-check-timer)
(test-assert "Timer initially stopped" (null consult-snapfile--stale-check-timer))
(consult-snapfile--start-stale-check-timer)
(test-assert "Timer started" (timerp consult-snapfile--stale-check-timer))
(consult-snapfile--stop-stale-check-timer)
(test-assert "Timer stopped" (null consult-snapfile--stale-check-timer))

;; Summary
(princ (format "\n=== Results: %d passed, %d failed ===\n"
               test-passed test-failed))

(kill-emacs (if (> test-failed 0) 1 0))

;;; test-reconnect.el ends here
