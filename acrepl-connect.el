;;; acrepl-connect.el --- connection -*- lexical-binding: t; -*-

;;; Commentary:

;;; Usage:
;;
;;  To perform some action automatically upon connection, define a hook
;;  function and add it to `acrepl-conn-success-hook:
;;
;;    (add-hook 'acrepl-conn-success-hook
;;              (lambda ()
;;                (insert ":hi")
;;                (comint-send-input)))

;;; Code:

;;;; Requirements

(require 'acrepl-state)

(require 'rx)
(require 'subr-x)

(defcustom acrepl-default-endpoint "localhost:23579"
  "Default host and port to connect to.
Host and port should be delimited with ':'."
  :type 'string
  :group 'acrepl)

(defcustom acrepl-conn-success-hook '()
  "Functions to run upon successful connection."
  :type 'hook
  :group 'acrepl)

;; XXX: figure out whether to put in acrepl-context...
(defvar-local acrepl-reconnect-try-number 0
  "Current reconnect try number.
Tracked in repl buffer, buffer local.")

(defun acrepl-arrange-retry (repl-buffer conn-name sentinel retry)
  "May be arrange for a reconnection attempt for REPL-BUFFER.
Try to reconnect with the connection associated with CONN-NAME,
using SENTINEL, influenced by the description of RETRY.
`acrepl-reconnect-try-number' is used to track number of connection attempts."
  (let ((wait-secs (plist-get retry :wait))
        (max-tries (plist-get retry :max-tries)))
    (run-at-time wait-secs nil
      (lambda ()
        (when (not repl-buffer)
          (error "Repl-buffer is no longer: %S" repl-buffer))
        (with-current-buffer repl-buffer
          (if (>= acrepl-reconnect-try-number max-tries)
            (progn
              (setq acrepl-reconnect-try-number 0)
              (message "Giving up after %S attempts." max-tries))
            ;; try again...
            (setq acrepl-reconnect-try-number
              (1+ acrepl-reconnect-try-number))
            ;; XXX: why acrepl-reconnect and not acrepl-connect?
            (acrepl-reconnect conn-name sentinel retry)))))))

(defun acrepl-connect (conn-name &optional sentinel retry)
  "Try to connect using CONN-NAME.
Optional argument SENTINEL is a process sentinel.
Optional argument RETRY is a plist describing the retrying."
  (let* ((endpoint (acrepl-get-endpoint conn-name))
         (host (acrepl-get-host endpoint))
         (port (acrepl-get-port endpoint))
         ;; XXX: wrap in acrepl-state.el? conn -> repl-buffer?
         (repl-buffer (get-buffer conn-name))
         (repl-buffer-name (buffer-name repl-buffer))
         (repl-process-name repl-buffer-name))
    (with-temp-message (format "Connecting to socket REPL on '%s:%d'..."
                               host port)
      (when (not (buffer-live-p repl-buffer))
        (error "Buffer not alive for conn: %S" conn-name))
      (condition-case nil
          (let ((proc-buffer (make-comint-in-buffer repl-process-name
                                                    repl-buffer-name
                                                    (cons host port))))
            (when (not proc-buffer)
              (error "Failed to connect to %s:%d" host port))
            (when sentinel
              (let ((process (get-process repl-process-name)))
                (when (not process)
                  (error "Failed to acquire repl process"))
                (set-process-sentinel process sentinel)))
            (with-current-buffer repl-buffer
              (run-hooks 'acrepl-conn-success-hook))
            proc-buffer)
        (file-error ; handling connection refused
         (when retry
           (acrepl-arrange-retry repl-buffer conn-name sentinel retry))
         nil)))))

(defun acrepl-reconnect (conn-name &optional sentinel retry)
  "Try to connect to connection named CONN-NAME.
Tries to guess a reasonable default.
Optional argument SENTINEL should be a process sentinel.
Optional argument RETRY is a plist describing the retrying."
  (interactive
   (let* ((default (or (when-let ((file-path (buffer-file-name))
                                  (conn-name (acrepl-get-conn file-path)))
                         conn-name)
                       (let ((buffer-name (buffer-name (current-buffer))))
                         (when (acrepl-repl-buffer-name? buffer-name)
                           buffer-name))
                       ""))
          (input (completing-read "Connection: "
                                 (acrepl-conn-names)
                                 nil
                                 "confirm"
                                 default)))
     (if (equal input "")
       (user-error "No connection specified")
       (list input))))
  (acrepl-connect conn-name sentinel retry))

(defun acrepl-set-current-conn (conn-name)
  "Set current connection to the one named CONN-NAME."
  (interactive
   (if (not (buffer-file-name))
       (user-error "Please invoke when visiting a Clojure file")
     (let ((input (completing-read "Connection: "
                                   (acrepl-conn-names)
                                   nil
                                   "confirm")))
       (if (equal input "")
           (user-error "No connection specified")
         (list input)))))
  (let ((file-path (buffer-file-name)))
    (when (not file-path)
      (user-error "Expected to be in a code buffer"))
    (acrepl-update-conn-path! conn-name file-path)))

;;;###autoload
(defun acrepl-plain-connect (endpoint)
  "Start acrepl.
Query user for ENDPOINT which specifies the Clojure socket REPL
endpoint.  ENDPOINT is a string of the form: \"hostname:port\"."
  (interactive
   (if (not (buffer-file-name)) ; XXX: loose
       (user-error "Please invoke when visiting a Clojure file")
    (let ((endpoint acrepl-default-endpoint))
      (list
        (read-string (format "REPL endpoint (default '%s'): " endpoint)
          endpoint nil endpoint)))))
  (let* ((host (acrepl-get-host endpoint))
         (port (acrepl-get-port endpoint))
         (file-buffer (current-buffer))
         (file-path (buffer-file-name))
         (repl-buffer (get-buffer-create
                       (acrepl-make-repl-buffer-name file-path port)))
         (repl-buffer-name (buffer-name repl-buffer))
         (conn-name repl-buffer-name))
    ;; need this before acrepl-connect can work
    (acrepl-set-endpoint! conn-name host port)
    (with-current-buffer repl-buffer
      (let ((res-buffer (acrepl-connect conn-name)))
        (if (not res-buffer)
            (progn
              (acrepl-remove-endpoint! conn-name) ; XXX: failed, remove?
              (error "Failed to start acrepl"))
          (acrepl-update-conn-path! conn-name file-path)
          (acrepl-mode)
          (pop-to-buffer (current-buffer))
          (goto-char (point-max))
          (pop-to-buffer file-buffer))))))

(provide 'acrepl-connect)

;;; acrepl-connect.el ends here
