;;; acrepl-shadow.el --- shadow-cljs -*- lexical-binding: t; -*-

;;; Commentary:

;;; Usage:
;;
;;  Convenient connection to socket repl for shadow-cljs, and optionally
;;  auto-reconnect upon shadow-cljs restart:
;;
;;    (require 'acrepl-shadow)
;;    ;; nil or don't set if you don't want auto reconnection
;;    (setq acrepl-shadow-auto-reconnect t)
;;
;;    M-x acrepl-shadow-connect

;;; Code:

;;;; Requirements

(require 'acrepl-connect)
(require 'acrepl-util)

(require 'filenotify)
(require 'subr-x)

(defvar acrepl-shadow-auto-reconnect nil
  "Attempt reconnection if shadow-cljs restarts.")

;;; XXX: git-specific
(defun acrepl-shadow-find-dot-dir ()
  "Find .shadow-cljs directory."
  (let ((closest-dot-git-parent
         (locate-dominating-file default-directory ".git")))
    (when closest-dot-git-parent
      (let ((dot-shadow-cljs-dir (concat closest-dot-git-parent
                                   ".shadow-cljs")))
        (when (file-exists-p dot-shadow-cljs-dir)
          dot-shadow-cljs-dir)))))

(defun acrepl-shadow-auto-reconnect-setup (dot-dir file-buffer)
  "Arrange for auto-reconnect on shadow-cljs restart.
Monitor DOT-DIR for appropriate changes to trigger reconnection.
Only handle FILE-BUFFER's reconnection though."
  (let ((port-file (concat dot-dir
                     "/socket-repl.port")))
    (when (not (file-exists-p port-file))
      (error "Socket repl port file for shadow-cljs not found"))
    (file-notify-add-watch dot-dir (list 'change)
      (lambda (event)
        ;; actions: created, changed, deleted, stopped
        (let ((action (nth 1 event))
              (file (nth 2 event)))
          (if (and (string-equal (expand-file-name file)
                     (expand-file-name port-file))
                (equal action 'changed))
            (let ((port (acrepl-number-from-file file)))
              (when (not (buffer-live-p file-buffer))
                (error "Missing buffer for: %s" file-buffer))
              (with-current-buffer file-buffer
                (when-let ((conn-name acrepl-connection-name))
                  (when-let ((conn (acrepl-get-connection conn-name)))
                    (let ((host (alist-get :host conn))
                          (port (alist-get :port conn)))
                      ;; XXX: may need more tweaking
                      (when (or (string-equal host "localhost")
                              (string-equal host "127.0.0.1")
                              (string-equal host "::1"))
                        (acrepl-connect conn)))))))))))))

(defun acrepl-shadow-connect ()
  "Start acrepl for a file in a shadow-cljs project."
  (interactive)
  (when (not (buffer-file-name)) ; XXX: loose
    (user-error "Please invoke when visiting a Clojure file"))
  (let ((dot-dir (acrepl-shadow-find-dot-dir)))
    (when (not dot-dir)
      (error "Failed to find .shadow-cljs directory"))
    (let ((port-file (concat dot-dir
                       "/socket-repl.port")))
      (when (not (file-exists-p port-file))
        (error "Socket repl port file for shadow-cljs not found"))
      (let ((port (acrepl-number-from-file port-file)))
        (when (not (> port 0))
          (error "Port was not > 0"))
        (let* ((host "localhost") ; XXX: ever be remote?
               (file-buffer (current-buffer))
               (file-path (buffer-file-name))
               (repl-buffer (get-buffer-create
                              (acrepl-make-repl-buffer-name file-path port)))
               (repl-buffer-name (buffer-name repl-buffer))
               (conn-name repl-buffer-name)
               (conn-desc
                 (acrepl-make-conn-desc conn-name host port file-path
                   (format-time-string "%Y-%m-%d_%H:%M:%S")
                   repl-buffer)))
          (setq acrepl-connection-name conn-name)
          (with-current-buffer repl-buffer
            (let ((res-buffer (acrepl-connect conn-desc)))
              (when (not res-buffer)
                (error "Failed to start acrepl"))
              (when acrepl-shadow-auto-reconnect
                (acrepl-shadow-auto-reconnect-setup dot-dir file-buffer))
              (acrepl-remember-connection conn-name conn-desc)
              (acrepl-mode)
              (pop-to-buffer (current-buffer))
              (goto-char (point-max))
              (pop-to-buffer file-buffer))))))))
  
(provide 'acrepl-shadow)

;;; acrepl-shadow.el ends here
