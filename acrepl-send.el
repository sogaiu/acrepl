;;; acrepl-send.el --- sending -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;; Requirements

(require 'acrepl-bounds)
(require 'acrepl-guess)

(require 'comint)

(defun acrepl-send-to-repl-buffer (code-str repl-buffer)
  "Send CODE-STR to REPL-BUFFER."
  (let ((original-buffer (current-buffer)))
    (save-excursion
      (with-current-buffer repl-buffer
        (goto-char (point-max))
        (insert code-str)
        (comint-send-input)))
    (when (eq original-buffer repl-buffer)
      (goto-char (point-max)))))

(defun acrepl-send-code (code-str)
  "Send CODE-STR.
CODE-STR should be a Clojure form."
  (interactive "sCode: ")
  (let ((repl-buffer (acrepl-guess-repl-buffer)))
    (if (not repl-buffer)
        (error "Did not find repl buffer.  May be no connection?")
      (acrepl-send-to-repl-buffer code-str repl-buffer))))

(defun acrepl-send-region (start end)
  "Send a region bounded by START and END."
  (interactive "r")
  (acrepl-send-code (buffer-substring-no-properties start end)))

(defun acrepl-send-buffer ()
  "Send buffer content."
  (interactive)
  (acrepl-send-region (point-min) (point-max)))

(defun acrepl-send-expr-at-point (&optional ignore-uneval)
  "Send expression at point.
Optional arg IGNORE-UNEVAL, if non-nil, does not send a leading uneval (#_)."
  (interactive "P")
  (cl-destructuring-bind (start end) (acrepl-detect-clojure-expr-bounds)
    (when (and start end)
      (acrepl-send-region
       (if (and ignore-uneval
                (string-equal "#_"
                              (buffer-substring-no-properties start
                                                              (+ start 2))))
           (+ 2 start)
         start)
       end))))

(provide 'acrepl-send)

;;; acrepl-send.el ends here
