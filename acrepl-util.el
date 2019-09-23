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

(defun acrepl-plist-from-simple-edn-file (file-path)
  "Return a plist created by interpreting braces as parens in FILE-PATH.
FILE-PATH should be a 'simple' edn file not containing things like sets.
The hack being applied here is to convert a Clojure map which uses braces as
delimiters into an Elisp property list which uses parens as delimiters."
   (condition-case nil
     (read
       (with-temp-buffer
         (insert-file-contents file-path)
         (goto-char (point-min))
         (let ((braces-re (rx (group-n 1 (or "{" "}")))))
           (while (re-search-forward braces-re nil t)
             (let ((matched (match-string 1)))
               (replace-match (if (string-equal matched "{")
                                "("
                                ")")))))
         (buffer-substring-no-properties (point-min) (point-max))))
     (error nil)))

(provide 'acrepl-util)

;;; acrepl-util.el ends here
