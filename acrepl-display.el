;;; acrepl-display.el --- display repl elswhere -*- lexical-binding: t; -*-

;;; Commentary:

;;; Usage:
;;
;;  To customize what happens to the displayed output, try something like:
;;
;;    (add-hook 'acrepl-display-last-output-hook
;;                (lambda ()
;;                  (clojure-mode)
;;                  (zprint-mode)))
;;
;;  This should give syntax-highlighted pretty-printed results.
;;
;;  N.B. zprint-mode.el is necessary for this example, but clearly
;;  it's not the only possibility :)

;;; Code:

;;;; Requirements

(require 'acrepl-guess)

(require 'comint)

(defun acrepl-display-last-output-wrapper (where-fn)
  "Display last repl output influenced by WHERE-FN."
  (let ((repl-buffer (acrepl-guess-repl-buffer))
        output)
    (if (not repl-buffer)
        (error "Did not find repl buffer.  May be no connection?")
      ;; see comint.el
      (save-excursion
        (with-current-buffer repl-buffer
          (goto-char (process-mark (get-buffer-process repl-buffer)))
          (forward-line 0)
          (setq output
                (buffer-substring-no-properties comint-last-input-end
                                                (point)))))
      (let ((display-buffer (get-buffer-create
                             (generate-new-buffer
                              (format-time-string
                               "acrepl-%Y-%m-%d_%H:%M:%S")))))
        (funcall where-fn display-buffer)
        (insert output)
        (run-hooks 'acrepl-display-last-output-hook)))))

(defun acrepl-last-output-in-window ()
  "Display last repl output in current window."
  (interactive)
  (acrepl-display-last-output-wrapper
   (lambda (buffer)
     ;; XXX: `switch-to-buffer'?
     (pop-to-buffer-same-window buffer))))

(defun acrepl-last-output-in-other-window ()
  "Display last repl output in current window."
  (interactive)
  (acrepl-display-last-output-wrapper
   (lambda (buffer)
     (switch-to-buffer-other-window buffer))))

(defun acrepl-last-output-in-frame ()
  "Display last repl output in a frame."
  (interactive)
  (acrepl-display-last-output-wrapper
   (lambda (buffer)
     (select-frame (make-frame))
     (pop-to-buffer buffer)
     (delete-other-windows))))

(provide 'acrepl-display)

;;; acrepl-display.el ends here
