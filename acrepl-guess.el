;;; acrepl-guess.el --- guessing -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;; Requirements

(require 'acrepl-state)

(defun acrepl-guess-repl-buffer ()
  "Return associated repl buffer for the current code buffer.
Should be executed in the context of a code buffer."
  (let* ((code-path (buffer-file-name))
         (repl-buffer (acrepl-get-repl-buffer code-path)))
    (when (not repl-buffer)
      (with-temp-message
          "Did not find repl buffer.  Looking for alternative..."
        (run-hooks 'acrepl-guess-repl-hook))
      ;; now try again and take whatever comes back
      (setq repl-buffer (acrepl-get-repl-buffer code-path)))
    repl-buffer))

(provide 'acrepl-guess)

;;; acrepl-guess.el ends here
