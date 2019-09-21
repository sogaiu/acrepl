;;; acrepl-connect.el --- connection -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;; Requirements

(require 'filenotify)
(require 'rx)

(defcustom acrepl-default-endpoint "localhost:23579"
  "Default host and port to connect to.
Host and port should be delimited with ':'."
  :type 'string
  :group 'acrepl)

(defvar acrepl-connections
  (make-hash-table :test #'equal)
  "Hash table of acrepl connections.")

(defvar-local acrepl-connection-name nil
  "Current connection name.")

(defun acrepl-make-conn-desc (name host port path ts repl-buffer)
  "Create conn descriptor from NAME, HOST, PORT, PATH, TS, and REPL-BUFFER."
  (list
   (cons 'name name)
   (cons 'host host)
   (cons 'port port)
   (cons 'path path)
   (cons 'ts ts)
   (cons 'repl-buffer repl-buffer)))

(defun acrepl-get-connection (name)
  "Get connection named NAME."
  (gethash name acrepl-connections))

(defun acrepl-connection-names ()
  "Return list of connection names."
  (let ((names '()))
    (maphash (lambda (k v)
               (push k names))
             acrepl-connections)
    names))

(defun acrepl-set-connection (name)
  "Set current connection to the one named NAME."
  (interactive
   (let ((input (completing-read "Connection: "
                                 (acrepl-connection-names)
                                 nil
                                 "confirm")))
     (if (equal input "")
       (user-error "No connection specified")
       (list input))))
  (let ((conn (gethash name acrepl-connections)))
    (when conn
      (setq acrepl-connection-name name)
      conn)))

(defun acrepl-current-connection ()
  "Return current connection, if any."
  (acrepl-get-connection acrepl-connection-name))

(defun acrepl-remember-connection (name connection)
  "Remember CONNECTION named NAME."
  (puthash name connection acrepl-connections))

;;; XXX: git-specific and works only for shadow-cljs
(defun acrepl-guess-endpoint ()
  "Guess an endpoint."
  (let ((closest-dot-git-parent
         (locate-dominating-file default-directory ".git")))
    (when closest-dot-git-parent
      (let* ((dot-shadow-cljs-dir (concat closest-dot-git-parent
                                    ".shadow-cljs"))
             (socket-repl-port-file (concat dot-shadow-cljs-dir
                                      "/socket-repl.port")))
        (when (file-exists-p socket-repl-port-file)
          ;; XXX: still needs work
          (file-notify-add-watch dot-shadow-cljs-dir (list 'change)
            (lambda (event)
              ;; actions: created, changed, deleted, stopped
              (let ((action (nth 1 event))
                    (file (nth 2 event)))
                (if (and (string-equal (expand-file-name file)
                            (expand-file-name socket-repl-port-file))
                      (equal action 'changed))
                  (let ((sc-port (string-to-number
                                   (with-temp-buffer
                                     (insert-file-contents file)
                                     (buffer-string)))))
                    (maphash (lambda (name conn)
                               (let ((host (alist-get 'host conn))
                                     (port (alist-get 'port conn)))
                                 ;; XXX: may need more tweaking
                                 (when (or (string-equal host "localhost")
                                         (string-equal host "127.0.0.1"))
                                   (when (= sc-port port)
                                     (acrepl-connect conn)))))
                      acrepl-connections))))))
          (let ((port (string-to-number
                       (with-temp-buffer
                         (insert-file-contents socket-repl-port-file)
                         (buffer-string)))))
            (when (> port 0)
              (format "localhost:%s" port))))))))

(defvar acrepl-conn-counter 0
  "Number of connections made so far.")

(defun acrepl-make-repl-buffer-name (path port)
  "Create a unique-ish repl buffer name using PATH, PORT and other info."
  ;; sample: src/augistints/core.cljc -> (1) augistints (2) core.cljc
  (let ((re-parent-and-file (rx (0+ anything)
                                "/"
                                (group-n 1 (1+ (not (any "/")))) ; parent
                                "/"
                                (group-n 2 (1+ (not (any "/")))) ; file
                                eol)))
    (if (not (string-match re-parent-and-file path))
        (error "Failed to parse: %s" path)
      ;; XXX: checking?
      (let ((dir-name (match-string 1 path))
            (file-name (match-string 2 path)))
        (setq acrepl-conn-counter (1+ acrepl-conn-counter))
        (format "[%s]*%s/%s*[%s]"
                acrepl-conn-counter
                (substring dir-name 0 (min 3 (length dir-name)))
                file-name
                port)))))

(defun acrepl-repl-buffer-name? (buffer-name)
  "Check if `acrepl-make-repl-buffer-name' could have made BUFFER-NAME."
  ;; sample: [1]*aug/core.cljc*[23579]
  (let ((re-rbn (rx bol
                    "[" (1+ digit) "]" ; conn counter
                    "*" (1+ (not (any "/"))) "/" (1+ (not (any "/"))) "*"
                    "[" (1+ digit) "]" ; port
                    eol)))
  (string-match re-rbn buffer-name)))
  
(defun acrepl-connect (conn-desc)
  "Try to connect using CONN-DESC."
  (let* ((name (alist-get 'name conn-desc))
         (host (alist-get 'host conn-desc))
         (port (alist-get 'port conn-desc))
         (repl-buffer (alist-get 'repl-buffer conn-desc))
         (repl-buffer-name (buffer-name repl-buffer))
         (repl-process-name repl-buffer-name))
    (message "Connecting to socket REPL on '%s:%d'..." host port)
    (if (not (buffer-live-p repl-buffer))
      (error "Buffer not alive? %S" name)
      (let ((buffer (make-comint-in-buffer repl-process-name repl-buffer-name
                      (cons host port))))
        (if (not buffer)
          (error "Failed to connect to %s:%d"  host port)
          buffer)))))

(defun acrepl-reconnect (name)
  "Try to connect to connection named NAME.
Tries to guess a reasonable default.
If `acrepl-connection-name' is set, assumes current buffer is a file
with Clojure code, and uses the value of the variable as the default.
Otherwise, if the current buffer name looks like an acrepl repl buffer
name, uses that as a default.
If neither of those things works out, just uses an empty string as the
default."
  (interactive
   (let* ((default (or acrepl-connection-name
                       (let ((buffer-name (buffer-name (current-buffer))))
                         (when (acrepl-repl-buffer-name? buffer-name)
                           buffer-name))
                       ""))
          (input (completing-read "Connection: "
                                 (acrepl-connection-names)
                                 nil
                                 "confirm"
                                 default)))
     (if (equal input "")
       (user-error "No connection specified")
       (list input))))
  (let ((conn-desc (gethash name acrepl-connections)))
    (when conn-desc ; XXX: errors?
      (acrepl-connect conn-desc))))

(provide 'acrepl-connect)

;;; acrepl-connect.el ends here
