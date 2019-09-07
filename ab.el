;;; ab.el --- Ascertain Bounds -*- lexical-binding: t; -*-

;; Author: sogaiu
;; Version: 20190907
;; Package-Requires: ((emacs "26.2"))
;; Keywords: bounds

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Code related to ascertaining the bounds of expressions.

;;;; Installation

;;;;; Manual

;; Put this file in your load-path, and put this in your init
;; file:

;; (require 'ab)

;;;; Usage

;; Run one of these commands:

;; `ab-select-elisp-def*': try to select various Emacs Lisp def* expressions
;; `ab-set-frame-height-to-elisp-def*': match frame height to elisp def* expr
;; `ab-select-clojure-def*': try to select various Clojure def* expressions
;; `ab-set-frame-height-to-clojure-def*': match frame height to clj def* expr

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

(require 'seq)
(require 'thingatpt)

;;;; Variables

(defvar ab-elisp-def-forms
  (list 'cl-defgeneric
        'cl-defmethod
        'defun
        'defvar
        'defalias
        'defcustom
        'defconst
        'defsubst
        'defadvice
        'defgroup
        'defmacro
        'defface
        'define-derived-mode
        'define-minor-mode))

(defvar ab-clojure-def-forms
  (list 'def
        'defn
        'defn-
        'defmacro
        'defmethod
        'defmulti
        'defrecord
        'deftype
        'defprotocol
        'extend-protocol))

;;;; Functions

;; XXX: consider regular expression matching...
;; XXX: factoring bits lead to problems, hence this is verbose atm
(defun ab-sexp-bounds (targets)
  "Find bounds of closest ancestor sexp having initial symbol in TARGETS.

After placing point within a def in Clojure, try:

  (ab-sexp-bounds (list 'def))

or within a defn or defn-, try:

  (ab-sexp-bounds (list 'defn 'defn-))

or within a defun in Emacs Lisp, try:

  (ab-sexp-bounds (list 'defun))

Successful detection of the bounds should yield a list with 2 elements
representing the start and end."
  (letrec ((here (point))
           ;; have a monkey climb up the tree...
           (monkey (lambda ()
                     (backward-up-list 1 (point) (point))
                     (if (/= 40 (char-after)) ; 40 is open paren
                         (funcall monkey)
                       (forward-char)
                       (let ((sym-here (symbol-at-point)))
                         (if (seq-find (lambda (a-target)
                                         (eq a-target sym-here))
                                       targets)
                           (progn
                             (backward-char)
                             t)
                           (backward-char)
                           (funcall monkey)))))))
    (condition-case nil
	(if (and (= 40 (char-after)) ; point is at open paren
		 (save-excursion
		     (forward-char)
		     (let ((sym-here (symbol-at-point)))
		       (if (seq-find (lambda (a-target)
				       (eq a-target sym-here))
				     targets)
			   t
			 nil))))
	    (let ((start (point))
		  (end (save-excursion
			 (forward-sexp)
			 (point))))
	      (goto-char here) ; not actually necessary
	      (list start end))
          (when (funcall monkey) ; point is not at open paren
            (let ((start (point))
                  (end (save-excursion
			 (forward-sexp)
			 (point))))
              (goto-char here)
              (list start end))))
      (error (goto-char here)
             nil))))

;;;;; Commands

;;;###autoload
(defun ab-select-elisp-def* ()
  "Guess the bounds of some elisp def-form around point."
  (interactive)
  (condition-case err
      (cl-destructuring-bind (start end) (ab-sexp-bounds ab-elisp-def-forms)
	(set-mark start)
	(goto-char end)
	(activate-mark))
    (wrong-number-of-arguments
     (message "Failed to find containing defn form."))
    (error
     (message "Error: %s %s" (car err) (cdr err)))))

;;;###autoload
(defun ab-set-frame-height-to-elisp-def* ()
  "Set the frame height to the bounds of some elisp def-form around point."
  (interactive)
  (condition-case err
      (cl-destructuring-bind (start end) (ab-sexp-bounds ab-elisp-def-forms)
	(set-frame-height (selected-frame)
                          (+ (- (line-number-at-pos end)
                                (line-number-at-pos start))
                             3))
        (let ((here (point)))
          (delete-other-windows)
          (goto-char start)
          (recenter-top-bottom 0)
          (goto-char here)))
    (wrong-number-of-arguments
     (message "Failed to find containing defn form."))
    (error
     (message "Error: %s %s" (car err) (cdr err)))))

;;;###autoload
(defun ab-select-clojure-def* ()
  "Guess the bounds of some Clojure def-form around point."
  (interactive)
  (condition-case err
      (cl-destructuring-bind (start end) (ab-sexp-bounds ab-clojure-def-forms)
	(set-mark start)
	(goto-char end)
	(activate-mark))
    (wrong-number-of-arguments
     (message "Failed to find containing def* form."))
    (error
     (message "Error: %s %s" (car err) (cdr err)))))

;;;###autoload
(defun ab-set-frame-height-to-clojure-def* ()
  "Set the frame height to the bounds of some elisp def-form around point."
  (interactive)
  (condition-case err
      (cl-destructuring-bind (start end) (ab-sexp-bounds ab-clojure-def-forms)
	(set-frame-height (selected-frame)
                          (+ (- (line-number-at-pos end)
                                (line-number-at-pos start))
                             3))
        (let ((here (point)))
          (delete-other-windows)
          (goto-char start)
          (recenter-top-bottom 0)
          (goto-char here)))
    (wrong-number-of-arguments
     (message "Failed to find containing defn form."))
    (error
     (message "Error: %s %s" (car err) (cdr err)))))

;;;; Footer

(provide 'ab)

;;; ab.el ends here
