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

(require 'cl-lib)
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

(defvar consult-snapfile--current-cwd nil
  "Absolute path of the current search root.")

(defvar consult-snapfile--current-mode nil
  "Mode string for the current search request.")

(defun consult-snapfile-project-root ()
  "Return the current project root as an absolute path."
  (consult-snapfile--project-root))

(defun consult-snapfile--normalize-mode (mode)
  "Return MODE as a protocol string."
  (pcase mode
    ((or 'files "files" 'nil) "files")
    ((or 'dirs "dirs") "dirs")
    ((or 'paths "paths") "paths")
    (_ (error "consult-snapfile: Unknown mode %S" mode))))

(defun consult-snapfile--mode-label (mode)
  "Return a user-facing label for MODE."
  (pcase mode
    ("dirs" "Directory")
    ("paths" "Path")
    (_ "File")))

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
             (let ((files (mapcar (lambda (item)
                                    (let ((text (plist-get item :text))
                                          (indices (plist-get item :indices)))
                                      (if indices
                                          (consult-snapfile--highlight text indices)
                                        text)))
                                  items)))
               (setq consult-snapfile--results
                     (append consult-snapfile--results files))
               (when consult-snapfile--sink
                 (funcall consult-snapfile--sink 'flush)
                 (funcall consult-snapfile--sink
                          consult-snapfile--results))))))
        ('complete
         (setq consult-snapfile--complete t))
        ('error
         (message "consult-snapfile: %s" (plist-get msg :message)))))))

(defun consult-snapfile--search (query)
  "Search for entries matching QUERY."
  (when consult-snapfile--current-request-id
    (consult-snapfile--send
     `(:type "cancel" :id ,consult-snapfile--current-request-id)))

  (setq consult-snapfile--results nil)
  (setq consult-snapfile--complete nil)
  (setq consult-snapfile--current-request-id (consult-snapfile--uuid))

  (consult-snapfile--send
   `(:type "search"
     :id ,consult-snapfile--current-request-id
     :mode ,consult-snapfile--current-mode
     :query ,query
     :cwd ,consult-snapfile--current-cwd
     :options (:max_results ,consult-snapfile-max-results))))

(defun consult-snapfile--make-async-source (sink cwd mode)
  "Create an async source for SINK using CWD and MODE."
  (lambda (action)
    (pcase action
      ('setup
       (setq consult-snapfile--sink sink
             consult-snapfile--current-cwd cwd
             consult-snapfile--current-mode mode)
       (consult-snapfile--ensure-connected)
       (consult-snapfile-add-message-handler #'consult-snapfile--message-handler)
       (consult-snapfile--search "")
       (funcall sink 'setup))
      ('destroy
       (consult-snapfile-remove-message-handler #'consult-snapfile--message-handler)
       (setq consult-snapfile--current-request-id nil
             consult-snapfile--sink nil
             consult-snapfile--current-cwd nil
             consult-snapfile--current-mode nil)
       (funcall sink 'destroy))
      ((pred stringp)
       (consult-snapfile--search action))
      ('flush
       (funcall sink 'flush))
      ('get
       consult-snapfile--results)
      (_
       (funcall sink action)))))

(defun consult-snapfile--async-source (sink)
  "Compatibility async source for file searches.
SINK is the consult async sink."
  (consult-snapfile--make-async-source
   sink
   (consult-snapfile-project-root)
   "files"))

(defun consult-snapfile--file-state (root)
  "Return a preview state function rooted at ROOT."
  (let ((open (consult--temporary-files))
        (state (consult--file-state)))
    (lambda (action cand)
      (unless cand
        (funcall open))
      (when cand
        (setq cand (expand-file-name cand root)))
      (funcall state action cand))))

(defun consult-snapfile--state ()
  "Compatibility state function for file preview."
  (consult-snapfile--file-state (consult-snapfile-project-root)))

(cl-defun consult-snapfile-read (&key cwd (mode 'files) prompt history category state (require-match t))
  "Read a project entry from CWD using MODE.
MODE is one of `files', `dirs', or `paths'."
  (let* ((root (expand-file-name (or cwd (consult-snapfile-project-root))))
         (mode (consult-snapfile--normalize-mode mode))
         (default-directory root))
    (consult--read
     (lambda (sink)
       (consult-snapfile--make-async-source sink root mode))
     :prompt (or prompt
                 (format "%s [%s]: "
                         (consult-snapfile--mode-label mode)
                         (abbreviate-file-name root)))
     :sort nil
     :require-match require-match
     :category (or category
                   (when (equal mode "files")
                     'file))
     :state (or state
                (when (equal mode "files")
                  (consult-snapfile--file-state root)))
     :history history)))

;;;###autoload
(defun consult-snapfile ()
  "Find file in current project with cached file list and fuzzy matching."
  (interactive)
  (let* ((root (consult-snapfile-project-root))
         (prompt (format "File [%s]: " (abbreviate-file-name root)))
         (selected (consult-snapfile-read
                    :cwd root
                    :mode 'files
                    :prompt prompt
                    :history 'file-name-history
                    :category 'file
                    :state (consult-snapfile--file-state root)
                    :require-match t)))
    (find-file (expand-file-name (substring-no-properties selected) root))))

(provide 'consult-snapfile)
;;; consult-snapfile.el ends here
