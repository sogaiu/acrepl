;;; acrepl-util.el --- utils -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;; Requirements

(defun acrepl-number-from-file (file)
  "Read content of FILE and return as number."
  (string-to-number
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string))))
  
(provide 'acrepl-util)

;;; acrepl-util.el ends here
