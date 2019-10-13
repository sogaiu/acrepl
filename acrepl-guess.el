;;; acrepl-guess.el --- guessing -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;; Requirements

(require 'acrepl-state)

(defun acrepl-guess-helper ()
  "Helper function for guessing repl-buffer."
  (let ((buffer-name (buffer-name)))
    (if (acrepl-repl-buffer-name? buffer-name)
        (current-buffer)
      (when-let ((code-path (buffer-file-name)))
        (acrepl-get-repl-buffer code-path)))))

(defun acrepl-guess-repl-buffer ()
  "Return relevant repl-buffer.
If current buffer is a repl-buffer, return that.
Otherwise, if current buffer is a code buffer, try to find an associated
repl-buffer.
Otherwise, return nil."
  (let (repl-buffer (acrepl-guess-helper))
    (when (not repl-buffer)
      (with-temp-message
          "Did not find repl buffer.  Looking for alternative..."
        (run-hooks 'acrepl-guess-repl-hook))
      ;; now try again and take whatever comes back
      (setq repl-buffer (acrepl-guess-helper)))
    repl-buffer))

(provide 'acrepl-guess)

;;; acrepl-guess.el ends here
