;;; cljm-align.el --- alignment -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'cljm-motion)
(require 'cljm-tidy)

(require 'align)

(defcustom cljm-align-reader-conditionals nil
  "Whether to align reader conditionals, as if they were maps."
  :package-version '(cljm-mode . "5.10")
  :safe #'booleanp
  :type 'boolean)

(defcustom cljm-align-binding-forms
  '("let" "when-let" "when-some" "if-let" "if-some" "binding" "loop"
    "doseq" "for" "with-open" "with-local-vars" "with-redefs")
  "List of strings matching forms that have binding forms."
  :package-version '(cljm-mode . "5.1")
  :safe #'listp
  :type '(repeat string))

(defcustom cljm-align-cond-forms
  '("condp" "cond" "cond->" "cond->>" "case" "are"
    "clojure.core/condp" "clojure.core/cond" "clojure.core/cond->"
    "clojure.core/cond->>" "clojure.core/case" "clojure.test/are")
  "List of strings identifying cond-like forms."
  :package-version '(cljm-mode . "5.1")
  :safe #'listp
  :type '(repeat string))

(defvar cljm--beginning-of-reader-conditional-regexp
  "#\\?@(\\|#\\?("
  "Regexp denoting the beginning of a reader conditional.")

(defun cljm--position-for-alignment ()
  "Non-nil if the sexp around point should be automatically aligned.
This function expects to be called immediately after an
open-brace or after the function symbol in a function call.

First check if the sexp around point is a map literal, or is a
call to one of the vars listed in `cljm-align-cond-forms'.  If
it isn't, return nil.  If it is, return non-nil and place point
immediately before the forms that should be aligned.

For instance, in a map literal point is left immediately before
the first key; while, in a let-binding, point is left inside the
binding vector and immediately before the first binding
construct."
  (let ((point (point)))
    ;; Are we in a map?
    (or (and (eq (char-before) ?{)
             (not (eq (char-before (1- point)) ?\#)))
        ;; Are we in a reader conditional?
        (and cljm-align-reader-conditionals
             (looking-back cljm--beginning-of-reader-conditional-regexp
                           (- (point) 4)))
        ;; Are we in a cond form?
        (let* ((fun    (car (member (thing-at-point 'symbol)
                                    cljm-align-cond-forms)))
               (method (and fun (cljm--get-indent-method fun)))
               ;; The number of special arguments in the cond form is
               ;; the number of sexps we skip before aligning.
               (skip   (cond ((numberp method) method)
                             ((null method) 0)
                             ((sequencep method) (elt method 0)))))
          (when (and fun (numberp skip))
            (cljm-forward-logical-sexp skip)
            (comment-forward (point-max))
            fun)) ; Return non-nil (the var name).
        ;; Are we in a let-like form?
        (when (member (thing-at-point 'symbol)
                      cljm-align-binding-forms)
          ;; Position inside the binding vector.
          (cljm-forward-logical-sexp)
          (backward-sexp)
          (when (eq (char-after) ?\[)
            (forward-char 1)
            (comment-forward (point-max))
            ;; Return non-nil.
            t)))))

(defun cljm--find-sexp-to-align (end)
  "Non-nil if there's a sexp ahead to be aligned before END.
Place point as in `cljm--position-for-alignment'."
  ;; Look for a relevant sexp.
  (let ((found))
    (while (and (not found)
                (search-forward-regexp
                 (concat (when cljm-align-reader-conditionals
                           (concat cljm--beginning-of-reader-conditional-regexp
                                   "\\|"))
                         "{\\|("
                         (regexp-opt
                          (append cljm-align-binding-forms
                                  cljm-align-cond-forms)
                          'symbols))
                 end 'noerror))

      (let ((ppss (syntax-ppss)))
        ;; If we're in a string or comment.
        (unless (or (elt ppss 3)
                    (elt ppss 4))
          ;; Only stop looking if we successfully position
          ;; the point.
          (setq found (cljm--position-for-alignment)))))
    found))

(defconst cljm--align-separator-newline-regexp "^ *$")

(defcustom cljm-align-separator cljm--align-separator-newline-regexp
  "The separator that will be passed to `align-region' when performing vertical alignment."
  :package-version '(cljm-mode . "5.10")
  :type `(choice
          (const
           :tag
           "Make blank lines prevent vertical alignment from happening."
           ,cljm--align-separator-newline-regexp)
          (other
           :tag
           "Allow blank lines to happen within a vertically-aligned expression."
           'entire)))

(defcustom cljm-align-forms-automatically nil
  "If non-nil, vertically align some forms automatically.
Automatically means it is done as part of indenting code.  This
applies to binding forms (`cljm-align-binding-forms'), to cond
forms (`cljm-align-cond-forms') and to map literals.  For
instance, selecting a map a hitting \\<cljm-mode-map>`\\[indent-for-tab-command]'
will align the values like this:
    {:some-key 10
     :key2     20}"
  :package-version '(cljm-mode . "5.1")
  :safe #'booleanp
  :type 'boolean)

(defun cljm-align (beg end)
  "Vertically align the contents of the sexp around point.
If region is active, align it.  Otherwise, align everything in the
current \"top-level\" sexp.
When called from lisp code align everything between BEG and END."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (save-excursion
                   (let ((end (progn (end-of-defun)
                                     (point))))
                     (cljm-backward-logical-sexp)
                     (list (point) end)))))
  (setq end (copy-marker end))
  (save-excursion
    (goto-char beg)
    (while (cljm--find-sexp-to-align end)
      (let ((sexp-end (save-excursion
                        (backward-up-list)
                        (forward-sexp 1)
                        (point-marker)))
            (cljm-align-forms-automatically nil)
            (count 1))
        ;; For some bizarre reason, we need to `align-region' once for each
        ;; group.
        (save-excursion
          (while (search-forward-regexp "^ *\n" sexp-end 'noerror)
            (cl-incf count)))
        (dotimes (_ count)
          (align-region (point) sexp-end nil
                        `((cljm-align
                           (regexp . cljm--search-whitespace-after-next-sexp)
                           (group . 1)
                           (separate . ,cljm-align-separator)
                           (repeat . t)))
                        nil))
        ;; Reindent after aligning because of #360.
        (indent-region (point) sexp-end)))))

(provide 'cljm-align)

;;; cljm-align.el ends here
