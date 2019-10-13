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

(require 'acrepl-connect)

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

(defcustom acrepl-project-type-hook '()
  "Functions to determine what type of project file is part of."
  :type 'hook
  :group 'acrepl)

(defcustom acrepl-guess-repl-hook '()
  "Functions to guess a repl-buffer when lookup fails."
  :type 'hook
  :group 'acrepl)

(defvar-local acrepl-project-types '()
  "Plist of project types for source file buffer.")

(defvar acrepl-mode-map
  (let ((map (copy-keymap comint-mode-map)))
    map)
  "ACRepl mode map.")

(define-derived-mode acrepl-mode comint-mode "A Clojure REPL"
  "Major mode for acrepl.
\\{acrepl-mode-map}"
  :syntax-table clojure-mode-syntax-table
  ;; with font-lock enabled, at some point the following chain of calls
  ;; can come about:
  ;;
  ;;   `font-lock-fontify-region'
  ;;   ...
  ;;   `clojure--looking-at-non-logical-sexp'
  ;;   `comment-normalize-vars'
  ;;
  ;; the last of which has certain expectations regarding some comment-*
  ;; variables, which if not met, can make life unpleasant in the minibuffer.
  ;; thus:
  (setq comment-start ";")
  (setq comment-end "")
  (setq comint-prompt-regexp acrepl-prompt-regexp)
  (setq comint-prompt-read-only t)
  (setq mode-line-process '(":%s"))
  ;; XXX: can use setq-local instead?
  (set (make-local-variable 'font-lock-defaults)
       '(clojure-font-lock-keywords t)))

;;;###autoload
(defun acrepl ()
  "Start acrepl.
Try to automatically start acrepl accounting for project type."
  (interactive)
  (when (not (buffer-file-name)) ; XXX: loose
    (user-error "Please invoke when visiting a Clojure file"))
  (run-hooks 'acrepl-project-type-hook)
  (let ((proj-types (thread-last acrepl-project-types
                      (seq-map-indexed (lambda (elt idx)
                                         (when (= (% idx 2) 0)
                                           elt)))
                      (seq-filter (lambda (item) item)))))
    (cl-case (length proj-types)
      (0 (call-interactively 'acrepl-plain-connect))
      (1 (let* ((proj-type (car proj-types))
                (connect (thread-first acrepl-project-types
                           (plist-get proj-type)
                           (plist-get :connect))))
           (call-interactively (or connect 'acrepl-plain-connect))))
      (t (let ((proj-type-str
                (completing-read "Project type: " proj-types
                  nil "confirm")))
           (if (not proj-type-str)
             (call-interactively 'acrepl-plain-connect)
             (let ((connect (thread-first acrepl-project-types
                              (plist-get (intern proj-type-str))
                              (plist-get :connect))))
               (call-interactively connect))))))))

(provide 'acrepl)

;;; acrepl.el ends here
