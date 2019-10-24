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
;;
;;  For auto-detection of project for use with M-x acrepl, try:
;;
;;    (add-hook 'acrepl-project-type-hook
;;              'acrepl-shadow-cljs-type-p)
;;
;;  For single connection per project behavior, try:
;;
;;    (add-hook 'acrepl-guess-repl-hook
;;              'acrepl-shadow-reuse-same-project-repl)
;;
;;  To disable if already enabled, try:
;;
;;    (remove-hook 'acrepl-guess-repl-hook
;;                 'acrepl-shadow-reuse-same-project-repl)

;;; Code:

;;;; Requirements

(require 'acrepl-connect)
(require 'acrepl-util)

(require 'filenotify)
(require 'subr-x)

(defvar acrepl-shadow-auto-reconnect nil
  "Attempt reconnection if shadow-cljs restarts.")

(defun acrepl-shadow-cljs-dir (code-path)
  "Determine shadow-cljs directory for CODE-PATH."
  (when-let ((guess-path (locate-dominating-file code-path "shadow-cljs.edn")))
    (file-truename guess-path)))

(defun acrepl-shadow-cljs-project-p ()
  "Determine whether some containing directory is a shadow-cljs project."
  (when-let ((closest-dir-with-sc-edn
               (locate-dominating-file default-directory "shadow-cljs.edn")))
    closest-dir-with-sc-edn))

(defun acrepl-shadow-cljs-type-p ()
  "Record if current source file is part of a shadow-cljs project.
One use would be via `add-hook' with `acrepl-project-type-hook'."
  (when-let ((path (acrepl-shadow-cljs-project-p)))
    (setq acrepl-project-types
      (plist-put acrepl-project-types :shadow-cljs
        (list
          :path path
          :connect #'acrepl-shadow-connect)))))

(defun acrepl-shadow-find-dot-dir ()
  "Find .shadow-cljs directory."
  (when-let ((shadow-project-dir (acrepl-shadow-cljs-project-p)))
    (let ((dot-shadow-cljs-dir (concat shadow-project-dir ".shadow-cljs")))
      (when (file-exists-p dot-shadow-cljs-dir)
        dot-shadow-cljs-dir))))

(defun acrepl-shadow-auto-reconnect-setup (dot-dir conn-name)
  "Arrange for auto-reconnect on shadow-cljs restart.
Monitor DOT-DIR for appropriate changes to trigger reconnection.
Only handle CONN-NAME's reconnection though."
  (let ((port-file (concat dot-dir
                     "/socket-repl.port")))
    (when (not (file-exists-p port-file))
      (error "Socket repl port file for shadow-cljs not found"))
    (file-notify-add-watch dot-dir (list 'change)
      (lambda (event)
        ;; actions: created, changed, deleted, stopped
        (let ((action (nth 1 event))
              (file (nth 2 event)))
          (when (and (string-equal (file-truename (expand-file-name file))
                                   (file-truename (expand-file-name port-file)))
                     (equal action 'changed))
            (let ((port (acrepl-number-from-file file)))
              (when (not (> port 0))
                (error "Unexpected socket repl file content"))
              (when-let ((repl-buffer (get-buffer conn-name))) ; XXX: wrap?
                (acrepl-connect conn-name)))))))))

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
          (error "Unexpected socket repl file content"))
        (let* ((host "localhost") ; XXX: ever be remote?
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
              (when acrepl-shadow-auto-reconnect
                ;; XXX: if nil, indicate to user not successful?
                (when (not (acrepl-shadow-auto-reconnect-setup dot-dir
                             conn-name))
                  (message "Warning: failed to setup auto-reconnect.")))
              (acrepl-update-conn-path! conn-name file-path)
              (acrepl-mode)
              (pop-to-buffer (current-buffer))
              (goto-char (point-max))
              (pop-to-buffer file-buffer)))))))))

(defun acrepl-shadow-reuse-same-project-repl ()
  "Try to find an existing connection in same project.
Searches existing connections for a matching one and if successful,
attempts to set it for the current code buffer.
Enable use via `add-hook' and `acrepl-guess-repl-hook'."
  (let ((code-path (buffer-file-name)))
    (when-let ((sc-dir (acrepl-shadow-cljs-dir code-path)))
      ;; XXX: somehow put appropriate bits in acrepl-state.el
      (let ((path-to-conn (acrepl-get-path-to-conn))
            candidates)
        ;; collect any files in same shadow-cljs project
        (maphash (lambda (path conn)
                   (let ((a-sc-dir (acrepl-shadow-cljs-dir path)))
                     (when (string-equal sc-dir a-sc-dir)
                       (push (cons path conn) candidates))))
                 path-to-conn)
        ;; XXX: customize choice policy?
        ;; for now just choose first match, if any
        (when-let ((match (car candidates)))
          (let ((path (car match))
                (conn (cdr match)))
            (acrepl-set-conn! code-path conn)
            (acrepl-add-conn-user! conn code-path)))))))

;; XXX: for testing
;;(add-hook 'acrepl-guess-repl-hook
;;          'acrepl-shadow-reuse-same-project-repl)

;; XXX: for testing
;;(add-hook 'acrepl-project-type-hook
;;          'acrepl-shadow-cljs-type-p)

(provide 'acrepl-shadow)

;;; acrepl-shadow.el ends here
