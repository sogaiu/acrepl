;;; cljm-paredit.el --- paredit support -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (defvar paredit-space-for-delimiter-predicates)
  (defvar paredit-version)
  (defvar paredit-mode))

(require 'cljm-indent)

(defun cljm-space-for-delimiter-p (endp delim)
  "Prevent paredit from inserting useless spaces.
See `paredit-space-for-delimiter-predicates' for the meaning of
ENDP and DELIM."
  (or endp
      (not (memq delim '(?\" ?{ ?\( )))
      (not (or (derived-mode-p 'cljm-mode)
               (derived-mode-p 'cider-repl-mode)))
      (save-excursion
        (backward-char)
        (cond ((eq (char-after) ?#)
               (and (not (bobp))
                    (or (char-equal ?w (char-syntax (char-before)))
                        (char-equal ?_ (char-syntax (char-before))))))
              ((and (eq delim ?\()
                    (eq (char-after) ??)
                    (eq (char-before) ?#))
               nil)
              (t)))))

(defconst cljm--collection-tag-regexp
  "#\\(::[a-zA-Z0-9._-]*\\|:?\\([a-zA-Z0-9._-]+/\\)?[a-zA-Z0-9._-]+\\)"
  "Collection reader macro tag regexp.
It is intended to check for allowed strings that can come before a
collection literal (e.g. '[]' or '{}'), as reader macro tags.
This includes #fully.qualified/my-ns[:kw val] and #::my-ns{:kw
val} as of Clojure 1.9.")

(defcustom cljm-omit-space-between-tag-and-delimiters '(?\[ ?\{ ?\()
  "Allowed opening delimiter characters after a reader literal tag.
For example, \[ is allowed in :db/id[:db.part/user]."
  :type '(set (const :tag "[" ?\[)
              (const :tag "{" ?\{)
              (const :tag "(" ?\()
              (const :tag "\"" ?\"))
  :safe (lambda (value)
          (and (listp value)
               (cl-every 'characterp value))))

(defun cljm-no-space-after-tag (endp delimiter)
  "Prevent inserting a space after a reader-literal tag.

When a reader-literal tag is followed be an opening delimiter
listed in `cljm-omit-space-between-tag-and-delimiters', this
function returns t.

This allows you to write things like #db/id[:db.part/user]
and #::my-ns{:some \"map\"} without inserting a space between
the tag and the opening bracket.

See `paredit-space-for-delimiter-predicates' for the meaning of
ENDP and DELIMITER."
  (if endp
      t
    (or (not (member delimiter cljm-omit-space-between-tag-and-delimiters))
        (save-excursion
          (let ((orig-point (point)))
            (not (and (re-search-backward
                       cljm--collection-tag-regexp
                       (line-beginning-position)
                       t)
                      (= orig-point (match-end 0)))))))))

(declare-function paredit-open-curly "ext:paredit" t t)
(declare-function paredit-close-curly "ext:paredit" t t)
(declare-function paredit-convolute-sexp "ext:paredit")

(defun cljm--in-string-p ()
  "Check whether the point is currently in a string."
  (nth 3 (syntax-ppss)))

(defvar cljm--let-regexp
  "\(\\(when-let\\|if-let\\|let\\)\\(\\s-*\\|\\[\\)"
  "Regexp matching let like expressions, i.e. \"let\", \"when-let\", \"if-let\".

The first match-group is the let expression.

The second match-group is the whitespace or the opening square
bracket if no whitespace between the let expression and the
bracket.")

(defun cljm--goto-let ()
  "Go to the beginning of the nearest let form."
  (when (cljm--in-string-p)
    (while (or (not (looking-at "("))
               (cljm--in-string-p))
      (backward-char)))
  (ignore-errors
    (while (not (looking-at cljm--let-regexp))
      (backward-up-list)))
  (looking-at cljm--let-regexp))

(defun cljm--sexp-regexp (sexp)
  "Return a regexp for matching SEXP."
  (concat "\\([^[:word:]^-]\\)"
          (mapconcat #'identity (mapcar 'regexp-quote (split-string sexp))
                     "[[:space:]\n\r]+")
          "\\([^[:word:]^-]\\)"))

(defun cljm--point-after (&rest actions)
  "Return POINT after performing ACTIONS.

An action is either the symbol of a function or a two element
list of (fn args) to pass to `apply''"
  (save-excursion
    (dolist (fn-and-args actions)
      (let ((f (if (listp fn-and-args) (car fn-and-args) fn-and-args))
            (args (if (listp fn-and-args) (cdr fn-and-args) nil)))
        (apply f args)))
    (point)))

(defun cljm--replace-sexp-with-binding (bound-name init-expr)
  "Replace a binding with its bound name in the let form.

BOUND-NAME is the name (left-hand side) of a binding.

INIT-EXPR is the value (right-hand side) of a binding."
  (save-excursion
    (while (re-search-forward
            (cljm--sexp-regexp init-expr)
            (cljm--point-after 'cljm--goto-let 'forward-sexp)
            t)
      (replace-match (concat "\\1" bound-name "\\2")))))

(defun cljm--replace-sexps-with-bindings (bindings)
  "Replace bindings with their respective bound names in the let form.

BINDINGS is the list of bound names and init expressions."
  (let ((bound-name (pop bindings))
        (init-expr (pop bindings)))
    (when bound-name
      (cljm--replace-sexp-with-binding bound-name init-expr)
      (cljm--replace-sexps-with-bindings bindings))))

(defun cljm--read-let-bindings ()
  "Read the bound-name and init expression pairs in the binding form.
Return a list: odd elements are bound names, even elements init expressions."
  (cljm--goto-let)
  (down-list 2)
  (let* ((start (point))
         (sexp-start start)
         (end (save-excursion
                (backward-char)
                (forward-sexp)
                (down-list -1)
                (point)))
         bindings)
    (while (/= sexp-start end)
      (forward-sexp)
      (push
       (string-trim (buffer-substring-no-properties sexp-start (point)))
       bindings)
      (skip-chars-forward "\r\n\t[:blank:]")
      (setq sexp-start (point)))
    (nreverse bindings)))

(defun cljm--replace-sexps-with-bindings-and-indent ()
  "Replace sexps with bindings."
  (cljm--replace-sexps-with-bindings
   (cljm--read-let-bindings))
  (cljm-indent-region
   (cljm--point-after 'clojure--goto-let)
   (cljm--point-after 'clojure--goto-let 'forward-sexp)))

(defun cljm--replace-let-bindings-and-indent ()
  "Replace let bindings and indent."
  (save-excursion
    (backward-sexp)
    (when (looking-back cljm--let-regexp nil)
      (cljm--replace-sexps-with-bindings-and-indent))))

(defun cljm-paredit-setup (&optional keymap)
  "Make \"paredit-mode\" play nice with `cljm-mode'.

If an optional KEYMAP is passed the changes are applied to it,
instead of to `cljm--mode-map'.
Also advice `paredit-convolute-sexp' when used on a let form as drop in
replacement for `cljr-expand-let`."
  (when (>= paredit-version 21)
    (let ((keymap (or keymap cljm-mode-map)))
      (define-key keymap "{" #'paredit-open-curly)
      (define-key keymap "}" #'paredit-close-curly))
    (add-to-list 'paredit-space-for-delimiter-predicates
                 #'cljm-space-for-delimiter-p)
    (add-to-list 'paredit-space-for-delimiter-predicates
                 #'cljm-no-space-after-tag)
    (advice-add 'paredit-convolute-sexp
                :after #'cljm--replace-let-bindings-and-indent)))

(provide 'cljm-paredit)

;;; cljm-paredit.el ends here
