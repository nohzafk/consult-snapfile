;;; consult-snapfile.el --- Fast file finder with caching for consult -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: nohzafk
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (consult "2.0") (websocket "1.14"))
;; Keywords: convenience, files, matching
;; URL: https://github.com/nohzafk/consult-snapfile

;; This file is not part of GNU Emacs.

;;; Commentary:

;; consult-snapfile provides a fast file finder powered by:
;; - fd for file discovery
;; - Server-side caching with file watching for instant results
;; - Nucleo fuzzy matching (fzf-style scoring)
;;
;; Usage:
;;   M-x consult-snapfile   - Find files in current project
;;
;; The server caches file lists per project and automatically
;; invalidates when files are added/deleted (via file watching).
;; This makes repeated searches instant (~2ms vs ~50ms).

;;; Code:

(require 'consult)
(require 'consult-snapfile-server)

(defgroup consult-snapfile nil
  "Fast file finder with caching."
  :group 'consult
  :prefix "consult-snapfile-")

(defcustom consult-snapfile-max-results 100
  "Maximum number of results to return."
  :type 'integer
  :group 'consult-snapfile)

(defvar consult-snapfile--current-request-id nil
  "ID of the current search request.")

(defvar consult-snapfile--results nil
  "Accumulated results for current search.")

(defvar consult-snapfile--sink nil
  "Sink function for async results.")

(defvar consult-snapfile--complete nil
  "Non-nil when search is complete.")

(defun consult-snapfile--highlight (text indices)
  "Apply highlight face to TEXT at character positions in INDICES.
INDICES can be a list or vector of character positions."
  (let ((result (copy-sequence text))
        (idx-list (if (vectorp indices) (append indices nil) indices)))
    (dolist (idx idx-list)
      (when (< idx (length result))
        (add-face-text-property idx (1+ idx) 'consult-highlight-match nil result)))
    result))

(defun consult-snapfile--message-handler (type msg)
  "Handle server message of TYPE with data MSG."
  (let ((id (plist-get msg :id)))
    (when (and id (equal id consult-snapfile--current-request-id))
      (pcase type
        ('results
         (let ((items (plist-get msg :items)))
           (when items
             ;; Convert items to file paths with highlighting
             (let ((files (mapcar (lambda (item)
                                    (let ((text (plist-get item :text))
                                          (indices (plist-get item :indices)))
                                      (if indices
                                          (consult-snapfile--highlight text indices)
                                        text)))
                                  items)))
               (setq consult-snapfile--results
                     (append consult-snapfile--results files))
               ;; Call sink with new results
               (when consult-snapfile--sink
                 (funcall consult-snapfile--sink 'flush)
                 (funcall consult-snapfile--sink consult-snapfile--results))))))
        ('complete
         (setq consult-snapfile--complete t))
        ('error
         (message "consult-snapfile: %s" (plist-get msg :message)))))))

(defun consult-snapfile--search (query)
  "Search for files matching QUERY."
  ;; Cancel previous request if any
  (when consult-snapfile--current-request-id
    (consult-snapfile--send `(:type "cancel" :id ,consult-snapfile--current-request-id)))

  ;; Reset state
  (setq consult-snapfile--results nil)
  (setq consult-snapfile--complete nil)
  (setq consult-snapfile--current-request-id (consult-snapfile--uuid))

  ;; Send search request
  (consult-snapfile--send
   `(:type "search"
     :id ,consult-snapfile--current-request-id
     :mode "files"
     :query ,query
     :cwd ,(consult-snapfile--project-root)
     :options (:max_results ,consult-snapfile-max-results))))

(defun consult-snapfile--async-source (sink)
  "Async source for consult. SINK is the consult async sink."
  (lambda (action)
    (pcase action
      ('setup
       ;; Store sink for message handler
       (setq consult-snapfile--sink sink)
       ;; Ensure server connection
       (consult-snapfile--ensure-connected)
       ;; Register our message handler
       (consult-snapfile-add-message-handler #'consult-snapfile--message-handler)
       ;; Initial search with empty query to populate cache
       (consult-snapfile--search "")
       ;; Pass setup to sink
       (funcall sink 'setup))
      ('destroy
       ;; Cleanup
       (consult-snapfile-remove-message-handler #'consult-snapfile--message-handler)
       (setq consult-snapfile--current-request-id nil)
       (setq consult-snapfile--sink nil)
       ;; Pass destroy to sink
       (funcall sink 'destroy))
      ((pred stringp)
       ;; Search query changed
       (consult-snapfile--search action))
      ('flush
       ;; Flush results to sink
       (funcall sink 'flush))
      ('get
       ;; Return current results
       consult-snapfile--results)
      (_
       ;; Pass other actions to sink
       (funcall sink action)))))

(defun consult-snapfile--state ()
  "State function for file preview."
  (let ((open (consult--temporary-files))
        (state (consult--file-state))
        (root (consult-snapfile--project-root)))
    (lambda (action cand)
      (unless cand
        (funcall open))
      ;; Expand relative path to absolute for consult--file-state
      (when cand
        (setq cand (expand-file-name cand root)))
      (funcall state action cand))))

;;;###autoload
(defun consult-snapfile ()
  "Find file in current project with cached file list and fuzzy matching."
  (interactive)
  (let* ((root (consult-snapfile--project-root))
         (default-directory root)
         (prompt (format "File [%s]: " (abbreviate-file-name root)))
         (selected (consult--read
                    #'consult-snapfile--async-source
                    :prompt prompt
                    :sort nil  ; Server already sorts by score
                    :require-match t
                    :category 'file
                    :state (consult-snapfile--state)
                    :history 'file-name-history)))
    ;; Expand relative path to absolute and strip text properties
    (find-file (expand-file-name (substring-no-properties selected) root))))

(provide 'consult-snapfile)
;;; consult-snapfile.el ends here
