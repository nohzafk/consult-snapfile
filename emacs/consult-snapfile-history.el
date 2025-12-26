;;; consult-snapfile-history.el --- Frecency tracking for consult-snapfile -*- lexical-binding: t -*-

;;; Commentary:

;; Track selection history to boost frequently/recently used items.
;; Frecency = frequency + recency (items used often AND recently rank higher)

;;; Code:

(require 'cl-lib)

(defgroup consult-snapfile-history nil
  "History and frecency settings for consult-snapfile."
  :group 'consult-snapfile
  :prefix "consult-snapfile-history-")

(defcustom consult-snapfile-history-file
  (expand-file-name "consult-snapfile-history.el" user-emacs-directory)
  "File to store selection history."
  :type 'file
  :group 'consult-snapfile-history)

(defcustom consult-snapfile-history-max-entries 1000
  "Maximum number of history entries to keep."
  :type 'integer
  :group 'consult-snapfile-history)

(defcustom consult-snapfile-history-frequency-weight 10
  "Score boost per selection count."
  :type 'integer
  :group 'consult-snapfile-history)

(defcustom consult-snapfile-history-recency-bonus 50
  "Extra score boost for items selected within recency window."
  :type 'integer
  :group 'consult-snapfile-history)

(defcustom consult-snapfile-history-recency-days 7
  "Number of days for recency bonus."
  :type 'integer
  :group 'consult-snapfile-history)

;; History data structure:
;; Hash table: key -> (count . last-selected-time)
(defvar consult-snapfile-history--data (make-hash-table :test 'equal)
  "History data: maps item key to (count . timestamp).")

(defvar consult-snapfile-history--loaded nil
  "Whether history has been loaded from disk.")

(defvar consult-snapfile-history--dirty nil
  "Whether history has unsaved changes.")

(defvar consult-snapfile-history--save-timer nil
  "Timer for debounced saving.")

;;; Core Functions

(defun consult-snapfile-history--ensure-loaded ()
  "Load history from disk if not already loaded."
  (unless consult-snapfile-history--loaded
    (consult-snapfile-history-load)
    (setq consult-snapfile-history--loaded t)))

(defun consult-snapfile-history-record (key &optional mode)
  "Record selection of KEY in MODE.
KEY is typically the item text, MODE is files/grep/fuzzy."
  (consult-snapfile-history--ensure-loaded)
  (let* ((full-key (if mode (format "%s:%s" mode key) key))
         (entry (gethash full-key consult-snapfile-history--data))
         (count (if entry (1+ (car entry)) 1))
         (now (float-time)))
    (puthash full-key (cons count now) consult-snapfile-history--data)
    (setq consult-snapfile-history--dirty t)
    (consult-snapfile-history--schedule-save)))

(defun consult-snapfile-history-get-boost (key &optional mode)
  "Get frecency score boost for KEY in MODE."
  (consult-snapfile-history--ensure-loaded)
  (let* ((full-key (if mode (format "%s:%s" mode key) key))
         (entry (gethash full-key consult-snapfile-history--data)))
    (if entry
        (let* ((count (car entry))
               (last-time (cdr entry))
               (days-ago (/ (- (float-time) last-time) 86400.0))
               (freq-boost (* count consult-snapfile-history-frequency-weight))
               (recency-boost (if (< days-ago consult-snapfile-history-recency-days)
                                  consult-snapfile-history-recency-bonus
                                0)))
          (+ freq-boost recency-boost))
      0)))

(defun consult-snapfile-history-boost-results (results &optional mode)
  "Apply frecency boost to RESULTS list, re-sort by boosted score.
Each result should be a plist with :text and :score.
MODE is files/grep/fuzzy for namespacing."
  (consult-snapfile-history--ensure-loaded)
  (let ((boosted (mapcar
                  (lambda (item)
                    (let* ((text (plist-get item :text))
                           (score (or (plist-get item :score) 0))
                           (boost (consult-snapfile-history-get-boost text mode))
                           (new-score (+ score boost)))
                      (plist-put (copy-sequence item) :score new-score)))
                  results)))
    ;; Sort by score descending
    (sort boosted (lambda (a b)
                    (> (plist-get a :score) (plist-get b :score))))))

;;; Persistence

(defun consult-snapfile-history-load ()
  "Load history from disk."
  (when (file-exists-p consult-snapfile-history-file)
    (condition-case err
        (with-temp-buffer
          (insert-file-contents consult-snapfile-history-file)
          (let ((data (read (current-buffer))))
            (when (hash-table-p data)
              (setq consult-snapfile-history--data data))))
      (error
       (message "consult-snapfile: Failed to load history: %s" (error-message-string err))
       (setq consult-snapfile-history--data (make-hash-table :test 'equal))))))

(defun consult-snapfile-history-save ()
  "Save history to disk."
  (when consult-snapfile-history--dirty
    (consult-snapfile-history--prune)
    (condition-case err
        (with-temp-file consult-snapfile-history-file
          (let ((print-length nil)
                (print-level nil))
            (prin1 consult-snapfile-history--data (current-buffer))))
      (error
       (message "consult-snapfile: Failed to save history: %s" (error-message-string err))))
    (setq consult-snapfile-history--dirty nil)))

(defun consult-snapfile-history--schedule-save ()
  "Schedule a debounced save."
  (when consult-snapfile-history--save-timer
    (cancel-timer consult-snapfile-history--save-timer))
  (setq consult-snapfile-history--save-timer
        (run-with-idle-timer 5 nil #'consult-snapfile-history-save)))

(defun consult-snapfile-history--prune ()
  "Remove old entries if over max."
  (when (> (hash-table-count consult-snapfile-history--data) consult-snapfile-history-max-entries)
    ;; Sort by last-time, remove oldest
    (let ((entries '()))
      (maphash (lambda (k v) (push (cons k v) entries)) consult-snapfile-history--data)
      (setq entries (sort entries (lambda (a b) (> (cddr a) (cddr b)))))
      (setq consult-snapfile-history--data (make-hash-table :test 'equal))
      (cl-loop for entry in entries
               for i from 0 below consult-snapfile-history-max-entries
               do (puthash (car entry) (cdr entry) consult-snapfile-history--data)))))

(defun consult-snapfile-history-clear ()
  "Clear all history."
  (interactive)
  (setq consult-snapfile-history--data (make-hash-table :test 'equal))
  (setq consult-snapfile-history--dirty t)
  (consult-snapfile-history-save)
  (message "consult-snapfile: History cleared"))

;; Save on Emacs exit
(add-hook 'kill-emacs-hook #'consult-snapfile-history-save)

(provide 'consult-snapfile-history)
;;; consult-snapfile-history.el ends here
