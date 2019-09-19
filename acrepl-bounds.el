;;; acrepl-ascertain.el --- s-expression bounds -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;; Requirements

(require 'seq)
(require 'thingatpt)

(defun acrepl-detect-clojure-expr-bounds ()
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

;; XXX: consider regular expression matching...
;; XXX: factoring bits lead to problems, hence this is verbose atm
(defun acrepl-expr-bounds (targets)
  "Find bounds of closest ancestor s-expr having initial symbol in TARGETS.
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

(provide 'acrepl-bounds)

;;; acrepl-bounds.el ends here
