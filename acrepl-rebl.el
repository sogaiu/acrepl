;;; acrepl-rebl.el --- rebl -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;; Requirements

(require 'acrepl-send)

(defun acrepl-start-rebl ()
  "Start a REBL UI."
  (interactive "")
  (acrepl-send-code
   (concat "(try\n"
           "  ((requiring-resolve 'cognitect.rebl/ui))\n"
           "  true\n"
           "  (catch Exception e nil))")))

;; XXX: consider a version that asks for a label
(defun acrepl-rebl-region (start end)
  "Send region bounded by START and END to REBL."
  (interactive "r")
  (let ((code-str (buffer-substring-no-properties start end)))
    (acrepl-send-code
     (concat "(try\n"
             "  ((requiring-resolve 'cognitect.rebl/submit)\n"
             "    (quote " code-str ")\n"
             "    " code-str ")\n"
             "  true\n"
             "  (catch Exception e nil))\n"))))

(defun acrepl-rebl-expr-at-point ()
  "Send expression at point to REBL."
  (interactive)
  (cl-destructuring-bind (start end) (acrepl-detect-clojure-expr-bounds)
    (when (and start end)
      (acrepl-rebl-region start end))))

(provide 'acrepl-rebl)

;;; acrepl-rebl.el ends here
