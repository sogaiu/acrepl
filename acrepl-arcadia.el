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

;;; Code:

;;;; Requirements

(require 'acrepl-connect)
(require 'acrepl-util)

(defvar acrepl-arcadia-auto-reconnect t
  "Attempt reconnection if disconnected from arcadia repl.")

(defun acrepl-arcadia-find-dir ()
  "Find arcadia directory with configuration.edn."
  ;; XXX: as insurance, could check for sibling Arcadia directory
  ;;      and/or Arcadia subdirectory
  (let ((closest-dot-git-parent
         (locate-dominating-file default-directory "Assets")))
    (when closest-dot-git-parent
      (let ((config-dir (concat closest-dot-git-parent
                          "Assets/Arcadia")))
        (when (file-exists-p config-dir)
          config-dir)))))

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
    ;; XXX
    (message "process: %S" process)
    (message "event: %S" event)
    ;; XXX: os-dependent?
    (cond ((string-prefix-p "connection broken by remote peer" event)
           (run-at-time 3 nil
             ;; XXX: should not just keep trying forever
             (lambda ()
               (when (not repl-buffer)
                 (error "Repl-buffer is no longer: %S" repl-buffer))
               (with-current-buffer repl-buffer
                 (acrepl-reconnect conn-name
                   #'acrepl-arcadia-reconnect-sentinel 'retrying)))))
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
  (let ((config-dir (acrepl-arcadia-find-dir)))
    (when (not config-dir)
      (error "Failed to find Arcadia directory"))
    (let ((config-file (concat config-dir
                         "/configuration.edn")))
      (let ((port 37220))
        (when (file-exists-p config-file)
          (let ((endpoint-plist (acrepl-arcadia-endpoint-from-cfg
                                  config-file)))
            (when endpoint-plist
              ;; XXX: ignoring :address for now
              (let ((port-from-file (plist-get endpoint-plist :port)))
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
          (setq acrepl-connection-name conn-name)
          (with-current-buffer repl-buffer
            (let ((res-buffer (acrepl-connect conn-desc
                                (when acrepl-arcadia-auto-reconnect
                                  #'acrepl-arcadia-reconnect-sentinel))))
              (when (not res-buffer)
                (error "Failed to start acrepl"))
              (acrepl-remember-connection conn-name conn-desc)
              (acrepl-mode)
              (pop-to-buffer (current-buffer))
              (goto-char (point-max))
              (pop-to-buffer file-buffer))))))))
  
(provide 'acrepl-arcadia)

;;; acrepl-arcadia.el ends here
