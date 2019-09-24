;;; acrepl-arcadia.el --- arcadia -*- lexical-binding: t; -*-

;;; Commentary:

;;; Usage:
;;
;;  Convenient connection to socket repl for arcadia, and optionally
;;  auto-reconnect.
;;
;;    (require 'acrepl-arcadia)
;;    ;; default is to try reconnection, so set to nil if undesired
;;    (setq acrepl-arcadia-auto-reconnect nil)
;;
;;    M-x acrepl-arcadia-connect
;;
;;  For auto-detection of project for use with M-x acrepl, try:
;;
;;    (add-hook 'acrepl-project-type-hook
;;              'acrepl-arcadia-type-p)

;;; Code:

;;;; Requirements

(require 'acrepl-connect)
(require 'acrepl-util)

(require 'subr-x)

(defvar acrepl-arcadia-auto-reconnect t
  "Attempt reconnection if disconnected from arcadia repl.")

(defvar acrepl-arcadia-reconnect-wait 3
  "Minimum number of seconds to wait before a retry.")

(defvar acrepl-arcadia-reconnect-max-retries 10
  "Maximum number of reconnection retries.")

(defun acrepl-arcadia-project-p ()
  "Determine whether some containing directory is an arcadia project."
  (when-let ((arcadia-parent
               (locate-dominating-file default-directory "Arcadia")))
    (when-let ((assets-parent
                 (locate-dominating-file arcadia-parent "Assets")))
      assets-parent)))

(defun acrepl-arcadia-type-p ()
  "Record if current source file is part of a arcadia project.
One use would be via `add-hook' with `acrepl-project-type-hook'."
  (when-let ((path (acrepl-arcadia-project-p)))
    (setq acrepl-project-types
      (plist-put acrepl-project-types :arcadia
        (list
          :path path
          :connect #'acrepl-arcadia-connect)))))

(defun acrepl-arcadia-find-config-dir ()
  "Find arcadia directory that should contain configuration.edn.
Assuming search starts in a source file somewhere within a subdirectory
of the Assets subdirectory of the unity project directory."
  (when-let ((arcadia-project-dir (acrepl-arcadia-project-p)))
    (let ((config-dir (concat arcadia-project-dir "Assets/Arcadia")))
      (when (file-exists-p config-dir)
        config-dir))))

;; some typical events (w/ last newline removed):
;;
;;   connection broken by remote peer <- :repl/quit at prompt
;;   deleted                          <- buffer killed
(defun acrepl-arcadia-reconnect-sentinel (process event)
  "Sentinel to help with reconnection.
PROCESS and EVENT are the usual arguments for sentinels."
  (let* ((repl-process-name (process-name process))
         (conn-name repl-process-name)
         (repl-buffer (get-buffer repl-process-name)))
    ;; XXX: should probably check validity of bound values...
    ;; XXX: os-dependent?
    (cond ((string-prefix-p "connection broken by remote peer" event)
           (run-at-time acrepl-arcadia-reconnect-wait nil
             ;; XXX: should not just keep trying forever
             (lambda ()
               (when (not repl-buffer)
                 (error "Repl-buffer is no longer: %S" repl-buffer))
               (with-current-buffer repl-buffer
                 (setq acrepl-arcadia-reconnect-try-number 1)
                 (acrepl-reconnect conn-name
                   #'acrepl-arcadia-reconnect-sentinel
                   (list
                     :max-tries acrepl-arcadia-reconnect-max-retries
                     :wait acrepl-arcadia-reconnect-wait))))))
          ((string-prefix-p "deleted" event)
           (message "buffer likely gone: %S" buffer))
          (t
           (message "Unrecognized event")
           (message "process: %S" process)))))

(defun acrepl-arcadia-endpoint-from-cfg (config-file)
  "Try to determine endpoint from arcadia CONFIG-FILE."
  (let ((config-plist (acrepl-plist-from-simple-edn-file config-file)))
    (when config-plist
      (plist-get config-plist :socket-repl))))
  
(defun acrepl-arcadia-connect ()
  "Start acrepl for a file in an arcadia project."
  (interactive)
  (when (not (buffer-file-name)) ; XXX: loose
    (user-error "Please invoke when visiting a Clojure file"))
  (let ((config-dir (acrepl-arcadia-find-config-dir)))
    (when (not config-dir)
      (error "Failed to find Arcadia directory"))
    (let ((config-file (concat config-dir
                         "/configuration.edn")))
      (let ((port 37220))
        (when (file-exists-p config-file)
          (let ((endpoint (acrepl-arcadia-endpoint-from-cfg
                            config-file)))
            (when (and endpoint (listp endpoint))
              ;; XXX: ignoring :address for now
              (let ((port-from-file (plist-get endpoint :port)))
                (when port-from-file
                  (setq port port-from-file))))))
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
          (setq acrepl-current-conn-name conn-name)
          (with-current-buffer repl-buffer
            (let ((res-buffer (acrepl-connect conn-desc
                                (when acrepl-arcadia-auto-reconnect
                                  #'acrepl-arcadia-reconnect-sentinel))))
              (when (not res-buffer)
                (error "Failed to start acrepl"))
              (acrepl-remember-conn conn-name conn-desc)
              (acrepl-mode)
              (pop-to-buffer (current-buffer))
              (goto-char (point-max))
              (pop-to-buffer file-buffer))))))))
  
(provide 'acrepl-arcadia)

;;; acrepl-arcadia.el ends here
