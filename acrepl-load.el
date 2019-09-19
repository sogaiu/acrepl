;;; acrepl-load.el --- loading -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;; Requirements

(require 'acrepl-send)

(defun acrepl-load-file (filename)
  "Send the `load-file` form with full path of FILENAME."
  (interactive "fFile name: ")
  (acrepl-send-code (format "(load-file \"%s\")"
                            (expand-file-name filename))))

(defun acrepl-load-buffer-file ()
  "Send the `load-file` form with buffer's full path."
  (interactive)
  (acrepl-load-file (buffer-file-name)))

(provide 'acrepl-load)

;;; acrepl-load.el ends here
