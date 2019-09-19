;;; acrepl-ascertain.el --- ascertaining -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;; Requirements

(require 'acrepl-bounds)

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
      (cl-destructuring-bind (start end) (acrepl-expr-bounds
                                          acrepl-ascertain-forms)
        (acrepl-send-region start end))
    (wrong-number-of-arguments
     (message "Failed to find containing def* form."))
    (error
     (message "Error: %s %s" (car err) (cdr err)))))

(provide 'acrepl-ascertain)

;;; acrepl-ascertain.el ends here
