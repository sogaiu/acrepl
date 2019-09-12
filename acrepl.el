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
;; dependencies) are in your load-path:
;;
;;   ab.el -- should be included in the same repository
;;   clojure-mode
;;
;;  and put this in your relevant init file:
;;
;;    (require 'acrepl)
;;
;;  Optionally, add:
;;
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

;; 1. Connect to the socket repl by:
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
;;    There should be a ACRepl menu containing some convenience commands:
;;
;;      Send ascertained region
;;      Send buffer
;;      Send expression at point
;;      Send region
;;      tap> expression at point
;;
;;      Load buffer file
;;      Load file
;;
;;      Switch to REPL

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

(require 'ab)
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

(defvar acrepl-repl-buffer-name "*acrepl-repl*"
  "Name of repl buffer.")

(defun acrepl-switch-to-repl ()
  "Switch to the repl buffer named by `acrepl-repl-buffer-name`."
  (interactive)
  (pop-to-buffer acrepl-repl-buffer-name))

(defun acrepl-send-code (code-str)
  "Send CODE-STR.

CODE-STR should be a Clojure form."
  (interactive "sCode: ")
  (let ((here (point))
        (original-buffer (current-buffer))
        (repl-buffer (get-buffer acrepl-repl-buffer-name)))
    (if (not repl-buffer)
        (message (format "%s is missing..." acrepl-repl-buffer-name))
      ;; switch to acrepl buffer to prepare for appending
      (set-buffer repl-buffer)
      (goto-char (point-max))
      (insert code-str)
      (comint-send-input)
      (set-buffer original-buffer)
      (if (eq original-buffer repl-buffer)
          (goto-char (point-max))
        (goto-char here)))))

(defun acrepl-send-region (start end &optional pre post)
  "Send a region bounded by START and END.

Optional arguments PRE and POST are strings to insert before
and after the region about to be sent, respectively."
  (interactive "r")
  (let ((here (point))
        (original-buffer (current-buffer))
        (repl-buffer (get-buffer acrepl-repl-buffer-name)))
    (if (not repl-buffer)
        (message (format "%s is missing..." acrepl-repl-buffer-name))
      ;; switch to acrepl buffer to prepare for appending
      (set-buffer repl-buffer)
      (goto-char (point-max))
      (when pre
        (insert pre))
      ;; switch back
      (set-buffer original-buffer)
      (append-to-buffer repl-buffer start end)
      (set-buffer repl-buffer)
      (when post
        (insert post))
      (comint-send-input)
      (set-buffer original-buffer)
      (goto-char here))))

(defun acrepl-tap-region (start end)
  "Apply tap> to a region bounded by START and END."
  (interactive "r")
  (acrepl-send-region start end "(tap> " ")"))

(defvar acrepl-ascertain-forms
  (list 'def
        'defn
        'defn-
        'defmacro
        'ns
        'require)
  "List of symbols used by `acrepl-send-ascertained-region'.")

(defun acrepl-send-ascertained-region ()
  "Send a region ascertained around point.
Determination is based on `acrepl-ascertain-forms'."
  (interactive)
  (condition-case err
      (cl-destructuring-bind (start end) (ab-sexp-bounds acrepl-ascertain-forms)
        (acrepl-send-region start end))
    (wrong-number-of-arguments
     (message "Failed to find containing def* form."))
    (error
     (message "Error: %s %s" (car err) (cdr err)))))

(defun acrepl-send-buffer ()
  "Send buffer content."
  (interactive)
  (acrepl-send-region (point-min) (point-max)))

;; XXX: experimental
(defun acrepl-detect-clojure-sexp-bounds ()
  "Return the bounds of the Clojure sexp at point."
  (let ((here (point)))
    (backward-sexp)
    (let ((start (if (looking-back "\\(#[^#)]*\\)")
                     (match-beginning 1)
                   (point))))
      (forward-sexp)
      (let ((end (point)))
        (goto-char here)
        (list start end)))))

(defun acrepl-send-expression-at-point ()
  "Send expression at point."
  (interactive)
  (cl-destructuring-bind (start end) (acrepl-detect-clojure-sexp-bounds)
    (when (and start end)
      (acrepl-send-region start end))))

(defun acrepl-tap-expression-at-point ()
  "Apply tap> to expression at point."
  (interactive)
  (cl-destructuring-bind (start end) (acrepl-detect-clojure-sexp-bounds)
    (when (and start end)
      (acrepl-tap-region start end))))

(defun acrepl-load-file (filename)
  "Send the `load-file` form with full path of FILENAME."
  (interactive "fFile name: ")
  (acrepl-send-code (format "(load-file \"%s\")"
                            (expand-file-name filename))))

(defun acrepl-load-buffer-file ()
  "Send the `load-file` form with buffer's full path."
  (interactive)
  (acrepl-load-file (buffer-file-name)))

(defvar acrepl-interaction-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-a" 'acrepl-send-ascertained-region)
    (define-key map "\C-c\C-b" 'acrepl-send-buffer)
    (define-key map "\C-c\C-e" 'acrepl-send-expression-at-point)
    (define-key map "\C-c\C-t" 'acrepl-tap-expression-at-point)
    (define-key map "\C-c\C-i" 'acrepl-load-file)
    (define-key map "\C-c\C-l" 'acrepl-load-buffer-file)
    (define-key map "\C-c\C-r" 'acrepl-send-region)
    (define-key map "\C-c\C-z" 'acrepl-switch-to-repl)
    (easy-menu-define acrepl-interaction-mode-map map
      "A Clojure REPL Interaction Mode Menu"
      '("ACRepl"
        ["Send ascertained region" acrepl-send-ascertained-region t]
        ["Send buffer" acrepl-send-buffer t]
        ["Send expression at point" acrepl-send-expression-at-point t]
        ["Send region" acrepl-send-region t]
        ["tap> expression at point" acrepl-tap-expression-at-point t]
        "--"
        ["Load buffer file" acrepl-load-buffer-file t]
        ["Load file" acrepl-load-file t]
        "--"
        ["Switch to REPL" acrepl-switch-to-repl t]))
    map)
  "ACRepl interaction mode map.")

(defvar acrepl-mode-map
  (let ((map (copy-keymap comint-mode-map)))
        (define-key map "\C-c\C-i" 'acrepl-load-file)
        (easy-menu-define acrepl-mode-map map
          "A Clojure REPL Mode Menu"
          '("ACRepl"
            ["Load file" acrepl-load-file t]))
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

;;;###autoload
(define-minor-mode acrepl-interaction-mode
  "Minor mode for acrepl interaction from a Clojure buffer.

The following keys are available in `acrepl-interaction-mode`:

\\{acrepl-interaction-mode}"

  nil " acrepl" acrepl-interaction-mode-map)

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

;;;###autoload
(defun acrepl (endpoint)
  "Start acrepl.

Query user for ENDPOINT which specifies the Clojure socket REPL
endpoint.  ENDPOINT is a string of the form: \"hostname:port\"."
  (interactive
   (let ((endpoint (or (acrepl-guess-endpoint)
                       acrepl-default-endpoint)))
     (list
      (read-string (format "REPL endpoint (default '%s'): " endpoint)
                   endpoint nil endpoint))))
  (unless
      ;(ignore-errors ;; XXX: uncomment at some point...
        (let* ((ep (split-string endpoint ":"))
               (host (car ep))
               (port (string-to-number (cadr ep))))
          (message "Connecting to socket REPL on '%s:%d'..." host port)
          (with-current-buffer (get-buffer-create acrepl-repl-buffer-name)
            (prog1
                (make-comint-in-buffer "acrepl" acrepl-repl-buffer-name
                                       (cons host port))
              (goto-char (point-max))
              (acrepl-mode)
              (pop-to-buffer (current-buffer))
              (goto-char (point-max)))))
    (message "Failed to connect to %s" endpoint)))

(provide 'acrepl)

;;; acrepl.el ends here
