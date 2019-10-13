;;; acrepl-tap.el --- tap -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;; Requirements

(require 'acrepl-send)

(defun acrepl-tap-region (start end)
  "Apply tap> to a region bounded by START and END."
  (interactive "r")
  (acrepl-send-code (concat "(tap> "
                            (buffer-substring-no-properties start end)
                            ")")))

(defun acrepl-tap-expr-at-point ()
  "Apply tap> to expression at point."
  (interactive)
  (cl-destructuring-bind (start end) (acrepl-detect-clojure-expr-bounds)
    (when (and start end)
      (acrepl-tap-region start end))))

(provide 'acrepl-tap)

;;; acrepl-tap.el ends here
