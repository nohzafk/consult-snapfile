;;; consult-snapfile-server.el --- Server connection for consult-snapfile -*- lexical-binding: t -*-

;;; Commentary:

;; Core connection and server management for consult-snapfile.

;;; Code:

;; Add vendor directory to load-path for bundled dependencies
(let ((vendor-dir (expand-file-name "vendor" (file-name-directory
                                               (or load-file-name buffer-file-name
                                                   (locate-library "consult-snapfile-server"))))))
  (when (file-directory-p vendor-dir)
    (add-to-list 'load-path vendor-dir)))

(require 'websocket)
(require 'json)

(defcustom consult-snapfile-server-port 9876
  "Port for the search server."
  :type 'integer
  :group 'consult-snapfile)

(defcustom consult-snapfile-server-host "127.0.0.1"
  "Host for the search server."
  :type 'string
  :group 'consult-snapfile)

(defcustom consult-snapfile-server-command nil
  "Command to start the search server.
If nil, tries to find the Rust binary in PATH or relative to this file.
Can be set to a list like '(\"consult-snapfile-server\" \"--port\" \"9876\")."
  :type '(choice (const nil) (repeat string))
  :group 'consult-snapfile)

(defcustom consult-snapfile-server-directory nil
  "Directory containing the server binary.
If nil, tries to find it in PATH or relative to this file."
  :type '(choice (const nil) directory)
  :group 'consult-snapfile)

(defcustom consult-snapfile-auto-reconnect t
  "Whether to automatically reconnect on connection drop."
  :type 'boolean
  :group 'consult-snapfile)

(defcustom consult-snapfile-reconnect-max-attempts 5
  "Maximum number of reconnection attempts before giving up."
  :type 'integer
  :group 'consult-snapfile)

(defcustom consult-snapfile-reconnect-base-delay 1.0
  "Base delay in seconds for exponential backoff."
  :type 'number
  :group 'consult-snapfile)

(defcustom consult-snapfile-reconnect-max-delay 30.0
  "Maximum delay in seconds between reconnection attempts."
  :type 'number
  :group 'consult-snapfile)

(defcustom consult-snapfile-request-timeout 30.0
  "Timeout in seconds for requests that never complete.
Requests older than this are considered stale and cleaned up."
  :type 'number
  :group 'consult-snapfile)

(defcustom consult-snapfile-stale-check-interval 10.0
  "Interval in seconds between checks for stale requests."
  :type 'number
  :group 'consult-snapfile)

(defvar consult-snapfile--connection nil
  "WebSocket connection to the server.")

(defvar consult-snapfile--server-process nil
  "Server process.")

(defvar consult-snapfile--request-handlers (make-hash-table :test 'equal)
  "Map of request ID to callback.")

(defvar consult-snapfile--message-handlers '()
  "List of message handler functions.")

(defvar consult-snapfile--reconnect-attempts 0
  "Current number of reconnection attempts.")

(defvar consult-snapfile--reconnect-timer nil
  "Timer for scheduled reconnection.")

(defvar consult-snapfile--connection-state 'disconnected
  "Current connection state: `disconnected', `connecting', or `connected'.")

(defvar consult-snapfile--intentional-disconnect nil
  "Non-nil if disconnect was intentional (user called stop).")

(defvar consult-snapfile--request-timestamps (make-hash-table :test 'equal)
  "Map of request ID to start timestamp (float-time).")

(defvar consult-snapfile--stale-check-timer nil
  "Timer for periodic stale request cleanup.")

;;; Server Management

(defun consult-snapfile--server-dir ()
  "Get the server directory."
  (or consult-snapfile-server-directory
      ;; Go up from emacs/ to project root
      (expand-file-name ".." (file-name-directory
                               (or load-file-name buffer-file-name
                                   (locate-library "consult-snapfile-server"))))))

(defun consult-snapfile--find-server-binary ()
  "Find the consult-snapfile-server binary."
  (or
   ;; 1. Check if in PATH
   (executable-find "consult-snapfile-server")
   ;; 2. Check relative to this file (target/release)
   (let ((bin-path (expand-file-name
                    "target/release/consult-snapfile-server"
                    (consult-snapfile--server-dir))))
     (when (file-executable-p bin-path)
       bin-path))
   ;; 3. Check relative to this file (target/debug)
   (let ((bin-path (expand-file-name
                    "target/debug/consult-snapfile-server"
                    (consult-snapfile--server-dir))))
     (when (file-executable-p bin-path)
       bin-path))))

;;;###autoload
(defun consult-snapfile-server-start ()
  "Start the consult-snapfile server."
  (interactive)
  (unless (and consult-snapfile--server-process (process-live-p consult-snapfile--server-process))
    (let* ((command (or consult-snapfile-server-command
                        (let ((binary (consult-snapfile--find-server-binary)))
                          (unless binary
                            (error "consult-snapfile-server binary not found. Install with: cargo build --release"))
                          (list binary))))
           (default-directory (consult-snapfile--server-dir)))
      (setq consult-snapfile--server-process
            (make-process
             :name "consult-snapfile-server"
             :buffer "*consult-snapfile-server*"
             :command command
             :sentinel #'consult-snapfile--server-sentinel)))
    ;; Wait a bit for server to start, then connect
    (run-with-timer 0.5 nil #'consult-snapfile-connect)))

;;;###autoload
(defun consult-snapfile-server-stop ()
  "Stop the consult-snapfile server."
  (interactive)
  (setq consult-snapfile--intentional-disconnect t)
  (consult-snapfile--cancel-reconnect)
  (when consult-snapfile--connection
    (websocket-close consult-snapfile--connection)
    (setq consult-snapfile--connection nil))
  (when (and consult-snapfile--server-process (process-live-p consult-snapfile--server-process))
    (kill-process consult-snapfile--server-process)
    (setq consult-snapfile--server-process nil))
  (setq consult-snapfile--connection-state 'disconnected))

(defun consult-snapfile--server-sentinel (process event)
  "Handle server PROCESS EVENT."
  (message "consult-snapfile server: %s" (string-trim event)))

;;; Reconnection Logic

(defun consult-snapfile--reconnect-delay ()
  "Calculate delay for next reconnection attempt using exponential backoff."
  (min consult-snapfile-reconnect-max-delay
       (* consult-snapfile-reconnect-base-delay
          (expt 2 consult-snapfile--reconnect-attempts))))

(defun consult-snapfile--schedule-reconnect ()
  "Schedule a reconnection attempt with exponential backoff."
  (consult-snapfile--cancel-reconnect)
  (if (>= consult-snapfile--reconnect-attempts consult-snapfile-reconnect-max-attempts)
      (progn
        (setq consult-snapfile--connection-state 'disconnected)
        (message "consult-snapfile: Max reconnection attempts reached. Use M-x consult-snapfile-connect to retry."))
    (let ((delay (consult-snapfile--reconnect-delay)))
      (setq consult-snapfile--connection-state 'connecting)
      (message "consult-snapfile: Reconnecting in %.1fs (attempt %d/%d)..."
               delay (1+ consult-snapfile--reconnect-attempts) consult-snapfile-reconnect-max-attempts)
      (setq consult-snapfile--reconnect-timer
            (run-with-timer delay nil #'consult-snapfile--do-reconnect)))))

(defun consult-snapfile--do-reconnect ()
  "Perform a reconnection attempt."
  (setq consult-snapfile--reconnect-timer nil)
  (setq consult-snapfile--reconnect-attempts (1+ consult-snapfile--reconnect-attempts))
  (consult-snapfile-connect))

(defun consult-snapfile--cancel-reconnect ()
  "Cancel any pending reconnection attempt."
  (when consult-snapfile--reconnect-timer
    (cancel-timer consult-snapfile--reconnect-timer)
    (setq consult-snapfile--reconnect-timer nil)))

(defun consult-snapfile--reset-reconnect-state ()
  "Reset reconnection state after successful connection."
  (consult-snapfile--cancel-reconnect)
  (setq consult-snapfile--reconnect-attempts 0)
  (setq consult-snapfile--intentional-disconnect nil))

(defun consult-snapfile--clear-pending-requests ()
  "Clear all pending request handlers."
  (let ((count (hash-table-count consult-snapfile--request-handlers)))
    (when (> count 0)
      (message "consult-snapfile: Clearing %d pending request(s)" count))
    (clrhash consult-snapfile--request-handlers)
    (clrhash consult-snapfile--request-timestamps)))

;;; Request Lifecycle Management

(defun consult-snapfile--register-request (id)
  "Register a new request with ID for lifecycle tracking."
  (puthash id (float-time) consult-snapfile--request-timestamps))

(defun consult-snapfile--complete-request (id)
  "Mark request ID as complete, removing it from tracking."
  (remhash id consult-snapfile--request-handlers)
  (remhash id consult-snapfile--request-timestamps))

(defun consult-snapfile--check-stale-requests ()
  "Check for and clean up stale requests that have timed out."
  (let ((now (float-time))
        (stale-ids '()))
    ;; Collect stale request IDs
    (maphash (lambda (id timestamp)
               (when (> (- now timestamp) consult-snapfile-request-timeout)
                 (push id stale-ids)))
             consult-snapfile--request-timestamps)
    ;; Clean up stale requests
    (dolist (id stale-ids)
      (let ((handler (gethash id consult-snapfile--request-handlers)))
        (when handler
          (message "consult-snapfile: Request %s timed out after %.1fs"
                   (substring id 0 8) consult-snapfile-request-timeout))
        (consult-snapfile--complete-request id)
        ;; Notify handlers of timeout
        (dolist (h consult-snapfile--message-handlers)
          (funcall h 'timeout `(:id ,id :message "Request timed out")))))))

(defun consult-snapfile--start-stale-check-timer ()
  "Start the periodic stale request check timer."
  (consult-snapfile--stop-stale-check-timer)
  (setq consult-snapfile--stale-check-timer
        (run-with-timer consult-snapfile-stale-check-interval
                        consult-snapfile-stale-check-interval
                        #'consult-snapfile--check-stale-requests)))

(defun consult-snapfile--stop-stale-check-timer ()
  "Stop the periodic stale request check timer."
  (when consult-snapfile--stale-check-timer
    (cancel-timer consult-snapfile--stale-check-timer)
    (setq consult-snapfile--stale-check-timer nil)))

;;; WebSocket Connection

;;;###autoload
(defun consult-snapfile-connect ()
  "Connect to the consult-snapfile server."
  (interactive)
  (consult-snapfile--cancel-reconnect)
  (when (and consult-snapfile--connection (websocket-openp consult-snapfile--connection))
    (websocket-close consult-snapfile--connection))
  (setq consult-snapfile--connection-state 'connecting)
  (setq consult-snapfile--intentional-disconnect nil)
  (condition-case err
      (setq consult-snapfile--connection
            (websocket-open
             (format "ws://%s:%d" consult-snapfile-server-host consult-snapfile-server-port)
             :on-message #'consult-snapfile--on-message
             :on-close #'consult-snapfile--on-close
             :on-error #'consult-snapfile--on-error
             :on-open #'consult-snapfile--on-open))
    (error
     (setq consult-snapfile--connection-state 'disconnected)
     (message "consult-snapfile: Failed to connect: %s" (error-message-string err))
     (when (and consult-snapfile-auto-reconnect (not consult-snapfile--intentional-disconnect))
       (consult-snapfile--schedule-reconnect)))))

(defun consult-snapfile--on-open (_ws)
  "Handle WebSocket open."
  (setq consult-snapfile--connection-state 'connected)
  (consult-snapfile--reset-reconnect-state)
  (consult-snapfile--start-stale-check-timer)
  (message "consult-snapfile: Connected to server"))

(defun consult-snapfile--on-close (_ws)
  "Handle WebSocket close."
  (setq consult-snapfile--connection nil)
  (setq consult-snapfile--connection-state 'disconnected)
  (consult-snapfile--stop-stale-check-timer)
  (consult-snapfile--clear-pending-requests)
  (if consult-snapfile--intentional-disconnect
      (message "consult-snapfile: Disconnected from server")
    (message "consult-snapfile: Connection lost")
    (when consult-snapfile-auto-reconnect
      (consult-snapfile--schedule-reconnect))))

(defun consult-snapfile--on-error (_ws _type err)
  "Handle WebSocket error ERR."
  (message "consult-snapfile: WebSocket error: %s" err))

(defun consult-snapfile--on-message (_ws frame)
  "Handle incoming WebSocket FRAME."
  (let* ((text (websocket-frame-text frame))
         (msg (condition-case nil
                  (json-parse-string text :object-type 'plist)
                (error nil))))
    (when msg
      (consult-snapfile--dispatch-message msg))))

(defun consult-snapfile--dispatch-message (msg)
  "Dispatch MSG to appropriate handler."
  (let ((type (plist-get msg :type)))
    (pcase type
      ("ready" (message "consult-snapfile: Server ready (v%s)" (plist-get msg :version)))
      ("pull" (consult-snapfile--handle-pull msg))
      ("results" (consult-snapfile--handle-results msg))
      ("complete" (consult-snapfile--handle-complete msg))
      ("error" (consult-snapfile--handle-error msg))
      (_ (message "consult-snapfile: Unknown message type: %s" type)))))

;;; Message Sending

(defun consult-snapfile--send (msg)
  "Send MSG to server as JSON."
  (when (and consult-snapfile--connection (websocket-openp consult-snapfile--connection))
    (websocket-send-text consult-snapfile--connection (json-serialize msg))))

(defun consult-snapfile--uuid ()
  "Generate a UUID."
  (format "%04x%04x-%04x-%04x-%04x-%04x%04x%04x"
          (random 65536) (random 65536)
          (random 65536)
          (logior (logand (random 65536) 4095) 16384)
          (logior (logand (random 65536) 16383) 32768)
          (random 65536) (random 65536) (random 65536)))

;;; Message Handlers

(defun consult-snapfile--handle-pull (msg)
  "Handle data pull request MSG from server."
  (require 'consult-snapfile-provider)
  (let* ((id (plist-get msg :id))
         (source (plist-get msg :source))
         (candidates (consult-snapfile-provider-get-candidates source)))
    (consult-snapfile--send
     `(:id ,id
       :type "pull-response"
       :source ,source
       :candidates ,(vconcat candidates)))))

(defun consult-snapfile--handle-results (msg)
  "Handle results MSG from server."
  (dolist (handler consult-snapfile--message-handlers)
    (funcall handler 'results msg)))

(defun consult-snapfile--handle-complete (msg)
  "Handle complete MSG from server."
  (let ((id (plist-get msg :id)))
    (when id
      (consult-snapfile--complete-request id)))
  (dolist (handler consult-snapfile--message-handlers)
    (funcall handler 'complete msg)))

(defun consult-snapfile--handle-error (msg)
  "Handle error MSG from server."
  (let ((id (plist-get msg :id)))
    (when id
      (consult-snapfile--complete-request id)))
  (message "consult-snapfile: Error: %s" (plist-get msg :message))
  (dolist (handler consult-snapfile--message-handlers)
    (funcall handler 'error msg)))

;;; Utilities

(defun consult-snapfile--ensure-connected ()
  "Ensure we are connected to the server, starting it if needed."
  (unless (and consult-snapfile--connection (websocket-openp consult-snapfile--connection))
    ;; First, try to connect (server might already be running externally)
    (condition-case nil
        (progn
          (consult-snapfile-connect)
          (sit-for 0.3))
      (error nil))
    ;; If still not connected, try starting the server
    (unless (and consult-snapfile--connection (websocket-openp consult-snapfile--connection))
      (unless (and consult-snapfile--server-process (process-live-p consult-snapfile--server-process))
        (consult-snapfile-server-start)
        ;; Wait for server to start
        (sit-for 0.6))
      ;; Try connecting again
      (unless (and consult-snapfile--connection (websocket-openp consult-snapfile--connection))
        (consult-snapfile-connect)
        (sit-for 0.3)))
    (unless (and consult-snapfile--connection (websocket-openp consult-snapfile--connection))
      (error "consult-snapfile: Failed to connect to server"))))

(defun consult-snapfile--project-root ()
  "Get the current project root as an absolute path."
  (expand-file-name
   (or (and (fboundp 'projectile-project-root)
            (projectile-project-root))
       (and (fboundp 'project-root)
            (when-let ((proj (project-current)))
              (project-root proj)))
       default-directory)))

(defun consult-snapfile-connection-state ()
  "Return current connection state: `disconnected', `connecting', or `connected'."
  consult-snapfile--connection-state)

(defun consult-snapfile-connected-p ()
  "Return non-nil if connected to server."
  (eq consult-snapfile--connection-state 'connected))

(defun consult-snapfile-add-message-handler (handler)
  "Add HANDLER to message handlers."
  (push handler consult-snapfile--message-handlers))

(defun consult-snapfile-remove-message-handler (handler)
  "Remove HANDLER from message handlers."
  (setq consult-snapfile--message-handlers (delete handler consult-snapfile--message-handlers)))

(provide 'consult-snapfile-server)
;;; consult-snapfile-server.el ends here
