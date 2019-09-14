;;; cljm-docstring.el --- docstring -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defcustom cljm-docstring-fill-column fill-column
  "Value of `fill-column' to use when filling a docstring."
  :type 'integer
  :safe 'integerp)

(defcustom cljm-docstring-fill-prefix-width 2
  "Width of `fill-prefix' when filling a docstring.
The default value conforms with the de facto convention for
Clojure docstrings, aligning the second line with the opening
double quotes on the third column."
  :type 'integer
  :safe 'integerp)

(defsubst cljm-docstring-fill-prefix ()
  "The prefix string used by `cljm-fill-paragraph'.
It is simply `cljm-docstring-fill-prefix-width' number of spaces."
  (make-string cljm-docstring-fill-prefix-width ? ))

(defsubst cljm-in-docstring-p ()
  "Check whether point is in a docstring."
  (let ((ppss (syntax-ppss)))
    ;; are we in a string?
    (when (nth 3 ppss)
      ;; check font lock at the start of the string
      (eq (get-text-property (nth 8 ppss) 'face)
          'font-lock-doc-face))))

;; Docstring positions
(put 'ns 'cljm-doc-string-elt 2)
(put 'def 'cljm-doc-string-elt 2)
(put 'defn 'cljm-doc-string-elt 2)
(put 'defn- 'cljm-doc-string-elt 2)
(put 'defmulti 'cljm-doc-string-elt 2)
(put 'defmacro 'cljm-doc-string-elt 2)
(put 'definline 'cljm-doc-string-elt 2)
(put 'defprotocol 'cljm-doc-string-elt 2)
(put 'deftask 'cljm-doc-string-elt 2) ;; common Boot macro

(provide 'cljm-docstring)

;;; cljm-docstring.el ends here
