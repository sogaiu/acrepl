;;; acrepl-guess.el --- guessing -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;; Requirements

(require 'acrepl-state)

(defun acrepl-guess-repl-buffer ()
  "Return relevant repl-buffer.
If current buffer is a repl-buffer, return that.
Otherwise, if current buffer is a code buffer, try to find an associated
repl-buffer.
Otherwise, return nil."
  (let* ((buffer-name (buffer-name))
         (repl-buffer (if (acrepl-repl-buffer-name? buffer-name)
                          (current-buffer)
                        (let ((code-path (buffer-file-name)))
                          (acrepl-get-repl-buffer code-path)))))
    (when (not repl-buffer)
      (with-temp-message
          "Did not find repl buffer.  Looking for alternative..."
        (run-hooks 'acrepl-guess-repl-hook))
      ;; now try again and take whatever comes back
      (setq repl-buffer (acrepl-get-repl-buffer code-path)))
    repl-buffer))

(provide 'acrepl-guess)

;;; acrepl-guess.el ends here
