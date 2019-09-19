;;; acrepl.el --- A Clojure REPL -*- lexical-binding: t; -*-

;; Author: sogaiu
;; Version: 20190907
;; Package-Requires: ((clojure-mode "5.11.0") (emacs "26.2"))
;; Keywords: clojure, repl

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A Clojure REPL - simple socket repl interaction and editor functionality

;;;; Installation

;;;;; Manual

;; Ensure this file and the following dependencies (and their
;; dependencies, etc.) are in your load-path:
;;
;;   clojure-mode
;;
;;  and put this in your relevant init file:
;;
;;    (require 'acrepl)
;;
;;  Optionally, add:
;;
;;    (require 'acrepl-interaction)
;;    (add-hook 'clojure-mode-hook
;;              #'acrepl-interaction-mode)
;;
;;  for editor features to be enabled when visiting a buffer with
;;  Clojure code in it.

;;;;; Automatic

;; TODO :)

;;;;; Usage

;; 0. Start up a clojure project (JVM, shadow-cljs, Arcadia, CLR) with a
;;    socket repl and note the host and port
;;
;;    N.B. for shadow-cljs', the info may be autodetected

;; 1. While visitng a buffer with Clojure code, connect to a socket repl by:
;;
;;      M-x acrepl
;;
;;    and at the prompt, specify a host and port like:
;;
;;      localhost:23579
;;
;;    i.e. a host or ip address followed by a colon and port number
;;
;;    A buffer for interaction with the socket repl should appear.

;; 2. For editor features, in a relevant buffer with a clojure source file:
;;
;;      M-x acrepl-interaction-mode
;;
;;    There should be a ACRepl menu containing some convenience
;;    commands related to sending to the repl, loading files, etc.
;;
;;    How featureful this ends up being can be controlled via:
;;
;;      (setq acrepl-interaction-menu-feature-level 0) ; default, simple
;;
;;    or:
;;
;;      (setq acrepl-interaction-menu-feature-level 1) ; gimme moar!
;;
;;    N.B. (require 'acrepl-interaction) is necessary.

;;;;; Acknowledgments

;; Thanks to those involved in:
;;
;;   cider
;;   clojure-mode
;;   inf-clojure
;;   miracle
;;   monroe
;;   replique
;;   sesman
;;
;; and transitively involved folks too ;)

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

;;;; Requirements

(require 'clojure-mode)
(require 'comint)
(require 'subr-x)

;;;; The Rest

(defgroup acrepl nil
  "A Clojure REPL"
  :prefix "acrepl-"
  :group 'applications)

;; clojure repls (NOT unrepl nor prepl):
;;
;;   my.namespace=>
;;
;; shadow-cljs:
;;
;;   [1:1] my.namespace=>
;;
;; note that each has a trailing space
(defcustom acrepl-prompt-regexp "^[^> \n]*=> *"
  "Regexp to recognize prompts in acrepl-mode."
  :type 'regexp
  :group 'acrepl)

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

(defun acrepl-make-conn-desc (host port path ts repl-buffer)
  "Create connection descriptor from HOST, PORT, PATH, TS, and REPL-BUFFER."
  (list
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

(defun acrepl-guess-repl-buffer ()
  "Return a relevant repl buffer."
  (let ((conn (acrepl-current-connection)))
    (when conn
      (alist-get 'repl-buffer conn)))) ; XXX: checking?

(defun acrepl-remember-connection (name connection)
  "Remember CONNECTION named NAME."
  (puthash name connection acrepl-connections))

(defun acrepl-switch-to-repl ()
  "Try to switch to a relevant repl buffer."
  (interactive)
  (let ((repl-buffer (acrepl-guess-repl-buffer)))
    (if (not repl-buffer)
        (error "Did not find repl buffer.  May be no connection?")
      (pop-to-buffer repl-buffer))))

(defvar acrepl-mode-map
  (let ((map (copy-keymap comint-mode-map)))
    map)
  "ACRepl mode map.")

(define-derived-mode acrepl-mode comint-mode "A Clojure REPL"
  "Major mode for acrepl.
\\{acrepl-mode-map}"
  :syntax-table clojure-mode-syntax-table
  (setq comint-prompt-regexp acrepl-prompt-regexp)
  (setq comint-prompt-read-only t)
  (setq mode-line-process '(":%s"))
  ;; XXX: can use setq-local instead?
  (set (make-local-variable 'font-lock-defaults)
       '(clojure-font-lock-keywords t)))

;;; XXX: git-specific and works only for shadow-cljs
(defun acrepl-guess-endpoint ()
  "Guess an endpoint."
  (let ((closest-dot-git-parent
         (locate-dominating-file default-directory ".git")))
    (when closest-dot-git-parent
      (let ((socket-repl-port-file (concat closest-dot-git-parent
                                           ".shadow-cljs/socket-repl.port")))
        (when (file-exists-p socket-repl-port-file)
          (let ((port (string-to-number
                       (with-temp-buffer
                         (insert-file-contents socket-repl-port-file)
                         (buffer-string)))))
            (when (> port 0)
              (format "localhost:%s" port))))))))

(defvar acrepl-conn-counter 0
  "Number of connections made so far.")

(defun acrepl-make-buffer-name (path port)
  "Create a unique-ish repl buffer name using PATH, PORT and other info."
  (string-match ".*/\\([^/]+\\)/\\([^/]+\\)$" path)
  ;; XXX: checking?
  (let ((dir-name (match-string 1 path))
        (file-name (match-string 2 path)))
    (setq acrepl-conn-counter (1+ acrepl-conn-counter))
    (format "[%s]*%s/%s*[%s]"
            acrepl-conn-counter
            (substring dir-name 0 (min 3 (length dir-name)))
            file-name
            port)))

;;;###autoload
(defun acrepl (endpoint)
  "Start acrepl.
Query user for ENDPOINT which specifies the Clojure socket REPL
endpoint.  ENDPOINT is a string of the form: \"hostname:port\"."
  (interactive
   (if (not (buffer-file-name)) ; XXX: loose
       (user-error "Please invoke when visiting a Clojure file")
     (let ((endpoint (or (acrepl-guess-endpoint)
                         acrepl-default-endpoint)))
       (list
        (read-string (format "REPL endpoint (default '%s'): " endpoint)
                     endpoint nil endpoint)))))
  (unless
      ;;(ignore-errors ;; XXX: uncomment at some point...
      (let* ((ep (split-string endpoint ":"))
             (host (car ep))
             (port (string-to-number (cadr ep)))
             (file-buffer (current-buffer))
             (file-path (buffer-file-name))
             (repl-buffer (get-buffer-create
                           (acrepl-make-buffer-name file-path port)))
             (repl-buffer-name (buffer-name repl-buffer))
             (conn-name repl-buffer-name)
             (conn-desc
              (acrepl-make-conn-desc host port file-path
                                     (format-time-string "%Y-%m-%d_%H:%M:%S")
                                     repl-buffer)))
        (message "Connecting to socket REPL on '%s:%d'..." host port)
        (setq acrepl-connection-name conn-name)
        (with-current-buffer repl-buffer
          (prog1
              (make-comint-in-buffer "acrepl" repl-buffer-name
                                     (cons host port))
            (acrepl-remember-connection conn-name conn-desc)
            (acrepl-mode)
            (pop-to-buffer (current-buffer))
            (goto-char (point-max))
            (pop-to-buffer file-buffer))))
    (message "Failed to connect to %s" endpoint)))

(provide 'acrepl)

;;; acrepl.el ends here
