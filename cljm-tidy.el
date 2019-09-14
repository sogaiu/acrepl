;;; cljm-tidy.el --- factored out for indent / align -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defvar cljm-get-indent-function nil
  "Function to get the indent spec of a symbol.
This function should take one argument, the name of the symbol as
a string.  This name will be exactly as it appears in the buffer,
so it might start with a namespace alias.

This function is analogous to the `cljm-indent-function'
symbol property, and its return value should match one of the
allowed values of this property.  See `cljm-indent-function'
for more information.")

(defun cljm--get-indent-method (function-name)
  "Return the indent spec for the symbol named FUNCTION-NAME.
FUNCTION-NAME is a string.  If it contains a `/', also try only
the part after the `/'.

Look for a spec using `cljm-get-indent-function', then try the
`cljm-indent-function' and `cljm-backtracking-indent'
symbol properties."
  (or (when (functionp cljm-get-indent-function)
        (funcall cljm-get-indent-function function-name))
      (get (intern-soft function-name) 'cljm-indent-function)
      (get (intern-soft function-name) 'cljm-backtracking-indent)
      (when (string-match "/\\([^/]+\\)\\'" function-name)
        (or (get (intern-soft (match-string 1 function-name))
                 'cljm-indent-function)
            (get (intern-soft (match-string 1 function-name))
                 'cljm-backtracking-indent)))
      ;; indent symbols starting with if, when, ...
      ;; such as if-let, when-let, ...
      ;; like if, when, ...
      (when (string-match (rx string-start (or "if" "when" "let" "while")
                              (syntax symbol))
                          function-name)
        (cljm--get-indent-method (substring
                                  (match-string 0 function-name) 0 -1)))))

(provide 'cljm-tidy)

;;; cljm-tidy.el ends here
