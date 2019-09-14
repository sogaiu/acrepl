;;; cljm-verify.el --- verify correct mode -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defcustom cljm-verify-major-mode t
  "If non-nil, warn when activating the wrong `major-mode'."
  :type 'boolean
  :safe #'booleanp
  :package-version '(cljm-mode "5.3.0"))

(defun cljm--check-wrong-major-mode ()
  "Check if the current `major-mode' matches the file extension.

If it doesn't, issue a warning if `cljm-verify-major-mode' is
non-nil."
  (when (and cljm-verify-major-mode
             (stringp (buffer-file-name)))
    (let* ((case-fold-search t)
           (problem (cond ((and (string-match "\\.clj\\'" (buffer-file-name))
                                (not (eq major-mode 'cljm-mode)))
                           'cljm-mode)
                          ((and (string-match "\\.cljs\\'" (buffer-file-name))
                                (not (eq major-mode 'cljms--mode)))
                           'cljms-mode)
                          ((and (string-match "\\.cljc\\'" (buffer-file-name))
                                (not (eq major-mode 'cljmc-mode)))
                           'cljmc-mode))))
      (when problem
        (message "[WARNING] %s activated `%s' instead of `%s' in this buffer.
This could cause problems.
\(See `cljm-verify-major-mode' to disable this message.)"
                 (if (eq major-mode real-this-command)
                     "You have"
                   "Something in your configuration")
                 major-mode
                 problem)))))

(add-hook 'cljm-mode-hook #'cljm--check-wrong-major-mode)

(provide 'cljm-verify)

;;; cljm-verify.el ends here
