;;; cljm.el --- Major mode for Clojure code -*- lexical-binding: t; -*-

;; Stripped down clojure-mode.el

;; Copyright © 2007-2019 Jeffrey Chu, Lennart Staflin, Phil Hagelberg
;; Copyright © 2013-2019 Bozhidar Batsov, Artur Malabarba
;;
;; Authors: Jeffrey Chu <jochu0@gmail.com>
;;       Lennart Staflin <lenst@lysator.liu.se>
;;       Phil Hagelberg <technomancy@gmail.com>
;;       Bozhidar Batsov <bozhidar@batsov.com>
;;       Artur Malabarba <bruce.connor.am@gmail.com>
;; URL: http://github.com/clojure-emacs/clojure-mode
;; Keywords: languages clojure clojurescript lisp
;; Version: 5.11.0
;; Package-Requires: ((emacs "25.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides font-lock, indentation, navigation
;; Clojure programming language (http://clojure.org).

;; Using clojure-mode with paredit or smartparens is highly recommended.

;; Here are some example configurations:

;;   ;; require or autoload paredit-mode
;;   (add-hook 'cljm-mode-hook #'paredit-mode)

;;   ;; require or autoload smartparens
;;   (add-hook 'cljm-mode-hook #'smartparens-strict-mode)

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; What appears to be left:

;; paragraph filling
;;
;; syntax highlighting via font-lock
;;
;; indentation / alignment
;;
;; paredit compatibility
;;
;; project root detection / project.el integration
;;
;; navigation to misc online docs
;;
;; syntax table
;;
;; outline
;;
;; sexp motion
;;
;; wrong major mode checking
;;
;; imenu
;;
;; misc function supporting above code

;;; Code:


(eval-when-compile
  (defvar calculate-lisp-indent-last-sexp)
  (defvar font-lock-beg)
  (defvar font-lock-end)
  (defvar paredit-space-for-delimiter-predicates)
  (defvar paredit-version)
  (defvar paredit-mode))

(require 'cl-lib)
(require 'imenu)
(require 'newcomment)
(require 'align)
(require 'subr-x)
(require 'lisp-mnt)
(require 'project)

(declare-function lisp-fill-paragraph  "lisp-mode" (&optional justify))

(defgroup cljm nil
  "Major mode for editing Clojure code."
  :prefix "cljm-"
  :group 'languages
  :link '(url-link :tag "GitHub" "https://github.com/sogaiu/cljm-mode")
  :link '(emacs-commentary-link :tag "Commentary" "cljm-mode"))

(defconst cljm-mode-version
  (eval-when-compile
    (lm-version (or load-file-name buffer-file-name)))
  "The current version of `cljm-mode'.")

(defface cljm-keyword-face
  '((t (:inherit font-lock-constant-face)))
  "Face used to font-lock Clojure keywords (:something)."
  :package-version '(cljm-mode . "3.0.0"))

(defface cljm-character-face
  '((t (:inherit font-lock-string-face)))
  "Face used to font-lock Clojure character literals."
  :package-version '(cljm-mode . "3.0.0"))

(defcustom cljm-indent-style 'always-align
  "Indentation style to use for function forms and macro forms.
There are two cases of interest configured by this variable.

- Case (A) is when at least one function argument is on the same
  line as the function name.
- Case (B) is the opposite (no arguments are on the same line as
  the function name).  Note that the body of macros is not
  affected by this variable, it is always indented by
  `lisp-body-indent' (default 2) spaces.

Note that this variable configures the indentation of function
forms (and function-like macros), it does not affect macros that
already use special indentation rules.

The possible values for this variable are keywords indicating how
to indent function forms.

    `always-align' - Follow the same rules as `lisp-mode'.  All
    args are vertically aligned with the first arg in case (A),
    and vertically aligned with the function name in case (B).
    For instance:
        (reduce merge
                some-coll)
        (reduce
         merge
         some-coll)

    `always-indent' - All args are indented like a macro body.
        (reduce merge
          some-coll)
        (reduce
          merge
          some-coll)

    `align-arguments' - Case (A) is indented like `lisp', and
    case (B) is indented like a macro body.
        (reduce merge
                some-coll)
        (reduce
          merge
          some-coll)"
  :safe #'symbolp
  :type '(choice
          (const :tag
                 "Same as `lisp-mode'" 'always-align)
          (const :tag
                 "Indent like a macro body" 'always-indent)
          (const :tag
                 "Indent like a macro body unless first arg is on the same line"
                 'align-arguments))
  :package-version '(cljm-mode . "5.2.0"))

(defcustom cljm-use-backtracking-indent t
  "When non-nil, enable context sensitive indentation."
  :type 'boolean
  :safe 'booleanp)

(defcustom cljm-max-backtracking 3
  "Maximum amount to backtrack up a list to check for context."
  :type 'integer
  :safe 'integerp)

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

(defcustom cljm-build-tool-files
  '("project.clj"      ; Leiningen
    "build.boot"       ; Boot
    "build.gradle"     ; Gradle
    "build.gradle.kts" ; Gradle
    "deps.edn"         ; Clojure CLI (a.k.a. tools.deps)
    "shadow-cljs.edn"  ; shadow-cljs
    )
  "A list of files, which identify a Clojure project's root.
Out-of-the box `cljm-mode' understands lein, boot, gradle,
 shadow-cljs and tools.deps."
  :type '(repeat string)
  :package-version '(cljm-mode . "5.0.0")
  :safe (lambda (value)
          (and (listp value)
               (cl-every 'stringp value))))

(defcustom cljm-project-root-function #'cljm-project-root-path
  "Function to locate clojure project root directory."
  :type 'function
  :risky t
  :package-version '(cljm-mode . "5.7.0"))

(defvar cljm-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map prog-mode-map)
    (define-key map (kbd "C-c SPC") #'cljm-align)
    (easy-menu-define cljm-mode-menu map "Clojure Mode Menu"
      '("Clojure"
        ["Align expression" cljm-align]
        ("Documentation"
         ["View a Clojure guide" cljm-view-guide]
         ["View a Clojure reference section" cljm-view-reference-section]
         ["View the Clojure cheatsheet" cljm-view-cheatsheet]
         ["View the Clojure style guide" cljm-view-style-guide])))
    map)
  "Keymap for Clojure mode.")

(defvar cljm-mode-syntax-table
  (let ((table (copy-syntax-table emacs-lisp-mode-syntax-table)))
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?? "_ p" table) ; ? is a prefix outside symbols
    (modify-syntax-entry ?# "_ p" table) ; # is allowed inside keywords (#399)
    (modify-syntax-entry ?~ "'" table)
    (modify-syntax-entry ?^ "'" table)
    (modify-syntax-entry ?@ "'" table)
    table)
  "Syntax table for Clojure mode.
Inherits from `emacs-lisp-mode-syntax-table'.")

(defconst cljm-guides-base-url "https://clojure.org/guides/"
  "The base URL for official Clojure guides.")

(defconst cljm-guides
  '(("Getting Started" . "getting_started")
    ("FAQ" . "faq")
    ("spec" . "spec")
    ("Destructuring" . "destructuring")
    ("Threading Macros" . "threading_macros")
    ("Comparators" . "comparators")
    ("Reader Conditionals" . "reader_conditionals"))
  "A list of all official Clojure guides.")

(defun cljm-view-guide ()
  "Open a Clojure guide in your default browser.

The command will prompt you to select one of the available guides."
  (interactive)
  (let ((guide (completing-read "Select a guide: "
                                (mapcar #'car cljm-guides))))
    (when guide
      (let ((guide-url (concat cljm-guides-base-url
                               (cdr (assoc guide cljm-guides)))))
        (browse-url guide-url)))))

(defconst cljm-reference-base-url "https://clojure.org/reference/"
  "The base URL for the official Clojure reference.")

(defconst cljm-reference-sections
  '(("The Reader" . "reader")
    ("The REPL and main" . "repl_and_main")
    ("Evaluation" . "evaluation")
    ("Special Forms" . "special_forms")
    ("Macros" . "macros")
    ("Other Functions" . "other_functions")
    ("Data Structures" . "data_structures")
    ("Datatypes" . "datatypes")
    ("Sequences" . "sequences")
    ("Transients" . "transients")
    ("Transducers" . "transducers")
    ("Multimethods and Hierarchies" . "multimethods")
    ("Protocols" . "protocols")
    ("Metadata" . "metadata")
    ("Namespaces" . "namespaces")
    ("Libs" . "libs")
    ("Vars and Environments" . "vars")
    ("Refs and Transactions" . "refs")
    ("Agents" . "agents")
    ("Atoms" . "atoms")
    ("Reducers" . "reducers")
    ("Java Interop" . "java_interop")
    ("Compilation and Class Generation" . "compilation")
    ("Other Libraries" . "other_libraries")
    ("Differences with Lisps" . "lisps")))

(defun cljm-view-reference-section ()
  "Open a Clojure reference section in your default browser.

The command will prompt you to select one of the available sections."
  (interactive)
  (let ((section (completing-read "Select a reference section: "
                                  (mapcar #'car cljm-reference-sections))))
    (when section
      (let ((section-url (concat cljm-reference-base-url
                                 (cdr (assoc section cljm-reference-sections)))))
        (browse-url section-url)))))

(defconst cljm-cheatsheet-url "https://clojure.org/api/cheatsheet"
  "The URL of the official Clojure cheatsheet.")

(defun cljm-view-cheatsheet ()
  "Open the Clojure cheatsheet in your default browser."
  (interactive)
  (browse-url cljm-cheatsheet-url))

(defconst cljm-style-guide-url "https://github.com/bbatsov/clojure-style-guide"
  "The URL of the Clojure style guide.")

(defun cljm-view-style-guide ()
  "Open the Clojure style guide in your default browser."
  (interactive)
  (browse-url cljm-style-guide-url))

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

(defvar cljm--let-regexp
  "\(\\(when-let\\|if-let\\|let\\)\\(\\s-*\\|\\[\\)"
  "Regexp matching let like expressions, i.e. \"let\", \"when-let\", \"if-let\".

The first match-group is the let expression.

The second match-group is the whitespace or the opening square
bracket if no whitespace between the let expression and the
bracket.")

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

(defun cljm-mode-variables ()
  "Set up initial buffer-local variables for Clojure mode."
  (add-to-list 'imenu-generic-expression '(nil cljm-match-next-def 0))
  (setq-local indent-tabs-mode nil)
  (setq-local paragraph-ignore-fill-prefix t)
  (setq-local outline-regexp ";;;\\(;* [^ \t\n]\\)\\|(")
  (setq-local outline-level 'lisp-outline-level)
  (setq-local comment-start ";")
  (setq-local comment-start-skip ";+ *")
  (setq-local comment-add 1) ; default to `;;' in comment-region
  (setq-local comment-column 40)
  (setq-local comment-use-syntax t)
  (setq-local multibyte-syntax-as-symbol t)
  (setq-local electric-pair-skip-whitespace 'chomp)
  (setq-local electric-pair-open-newline-between-pairs nil)
  (setq-local fill-paragraph-function #'cljm-fill-paragraph)
  (setq-local adaptive-fill-function #'cljm-adaptive-fill-function)
  (setq-local normal-auto-fill-function #'cljm-auto-fill-function)
  (setq-local comment-start-skip
              "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\)\\(;+\\|#|\\) *")
  (setq-local indent-line-function #'cljm-indent-line)
  (setq-local indent-region-function #'cljm-indent-region)
  (setq-local lisp-indent-function #'cljm-indent-function)
  (setq-local lisp-doc-string-elt-property 'cljm-doc-string-elt)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local open-paren-in-column-0-is-defun-start nil))

(defsubst cljm-in-docstring-p ()
  "Check whether point is in a docstring."
  (let ((ppss (syntax-ppss)))
    ;; are we in a string?
    (when (nth 3 ppss)
      ;; check font lock at the start of the string
      (eq (get-text-property (nth 8 ppss) 'face)
          'font-lock-doc-face))))

;;;###autoload
(define-derived-mode cljm-mode prog-mode "Clojure"
  "Major mode for editing Clojure code.

\\{cljm-mode-map}"
  (cljm-mode-variables)
  (cljm-font-lock-setup)
  (add-hook 'paredit-mode-hook #'cljm-paredit-setup)
  ;; `electric-layout-post-self-insert-function' prevents indentation in strings
  ;; and comments, force indentation of non-inlined docstrings:
  (add-hook 'electric-indent-functions
            (lambda (_char)
              (if (and (cljm-in-docstring-p)
                       ;; make sure we're not dealing with an inline docstring
                       ;; e.g. (def foo "inline docstring" bar)
                       (save-excursion
                         (beginning-of-line-text)
                         (eq (get-text-property (point) 'face)
                             'font-lock-doc-face)))
                  'do-indent)))
  ;; integration with project.el
  (add-hook 'project-find-functions #'cljm-current-project))

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

(defsubst cljm-docstring-fill-prefix ()
  "The prefix string used by `cljm-fill-paragraph'.
It is simply `cljm-docstring-fill-prefix-width' number of spaces."
  (make-string cljm-docstring-fill-prefix-width ? ))

(defun cljm-adaptive-fill-function ()
  "Clojure adaptive fill function.
This only takes care of filling docstring correctly."
  (when (cljm-in-docstring-p)
    (cljm-docstring-fill-prefix)))

(defun cljm-fill-paragraph (&optional justify)
  "Like `fill-paragraph', but can handle Clojure docstrings.
If JUSTIFY is non-nil, justify as well as fill the paragraph."
  (if (cljm-in-docstring-p)
      (let ((paragraph-start
             (concat paragraph-start
                     "\\|\\s-*\\([(:\"[]\\|~@\\|`(\\|#'(\\)"))
            (paragraph-separate
             (concat paragraph-separate "\\|\\s-*\".*[,\\.]$"))
            (fill-column (or cljm-docstring-fill-column fill-column))
            (fill-prefix (cljm-docstring-fill-prefix)))
        ;; we are in a string and string start pos (8th element) is non-nil
        (let* ((beg-doc (nth 8 (syntax-ppss)))
               (end-doc (save-excursion
                          (goto-char beg-doc)
                          (or (ignore-errors (forward-sexp) (point))
                              (point-max)))))
          (save-restriction
            (narrow-to-region beg-doc end-doc)
            (fill-paragraph justify))))
    (let ((paragraph-start (concat paragraph-start
                                   "\\|\\s-*\\([(:\"[]\\|`(\\|#'(\\)"))
          (paragraph-separate
           (concat paragraph-separate "\\|\\s-*\".*[,\\.[]$")))
      (or (fill-comment-paragraph justify)
          (fill-paragraph justify))
      ;; Always return `t'
      t)))

(defun cljm-auto-fill-function ()
  "Clojure auto-fill function."
  ;; Check if auto-filling is meaningful.
  (let ((fc (current-fill-column)))
    (when (and fc (> (current-column) fc))
      (let ((fill-column (if (cljm-in-docstring-p)
                             cljm-docstring-fill-column
                           fill-column))
            (fill-prefix (cljm-adaptive-fill-function)))
        (do-auto-fill)))))


;;; #_ comments font-locking
;; Code heavily borrowed from Slime.
;; https://github.com/slime/slime/blob/master/contrib/slime-fontifying-fu.el#L186
(defvar cljm--comment-macro-regexp
  (rx "#_" (* " ") (group-n 1 (not (any " "))))
  "Regexp matching the start of a comment sexp.
The beginning of match-group 1 should be before the sexp to be
marked as a comment.  The end of sexp is found with
`cljm-forward-logical-sexp'.")

(defvar cljm-reader-and-comment-regexp
  "#_ *\\(?1:[^ ]\\)\\|\\(?1:(comment\\_>\\)"
  "Regexp matching both `#_' macro and a comment sexp." )

(defcustom cljm-comment-regexp cljm--comment-macro-regexp
  "Comment mode.

The possible values for this variable are keywords indicating
what is considered a comment (affecting font locking).

    - Reader macro `#_' only - the default
    - Reader macro `#_' and `(comment)'"
  :type '(choice (const :tag "Reader macro `#_' and `(comment)'"
                        cljm-reader-and-comment-regexp)
                 (other :tag "Reader macro `#_' only"
                        cljm--comment-macro-regexp))
  :package-version '(cljm-mode . "5.7.0"))

(defun cljm-search-comment-macro-internal (limit)
  "Search for a comment forward stopping at LIMIT."
  (when (search-forward-regexp cljm-comment-regexp limit t)
    (let* ((md (match-data))
           (start (match-beginning 1))
           (state (syntax-ppss start)))
      ;; inside string or comment?
      (if (or (nth 3 state)
              (nth 4 state))
          (cljm-search-comment-macro-internal limit)
        (goto-char start)
        (cljm-forward-logical-sexp 1)
        ;; Data for (match-end 1).
        (setf (elt md 3) (point))
        (set-match-data md)
        t))))

(defun cljm-search-comment-macro (limit)
  "Find comment macros and set the match data.
Search from point up to LIMIT.  The region that should be
considered a comment is between `(match-beginning 1)'
and `(match-end 1)'."
  (let ((result 'retry))
    (while (and (eq result 'retry) (<= (point) limit))
      (condition-case nil
          (setq result (cljm-search-comment-macro-internal limit))
        (end-of-file (setq result nil))
        (scan-error  (setq result 'retry))))
    result))


;;; General font-locking
(defun cljm-match-next-def ()
  "Scans the buffer backwards for the next \"top-level\" definition.
Called by `imenu--generic-function'."
  ;; we have to take into account namespace-definition forms
  ;; e.g. s/defn
  (when (re-search-backward "^[ \t]*(\\([a-z0-9.-]+/\\)?\\(def\\sw*\\)" nil t)
    (save-excursion
      (let (found?
            (deftype (match-string 2))
            (start (point)))
        (down-list)
        (forward-sexp)
        (while (not found?)
          (ignore-errors
            (forward-sexp))
          (or (when (char-equal ?\[ (char-after (point)))
                (backward-sexp))
              (when (char-equal ?\) (char-after (point)))
                (backward-sexp)))
          (cl-destructuring-bind (def-beg . def-end)
              (bounds-of-thing-at-point 'sexp)
            (if (char-equal ?^ (char-after def-beg))
                (progn (forward-sexp) (backward-sexp))
              (setq found? t)
              (when (string= deftype "defmethod")
                (setq def-end (progn (goto-char def-end)
                                     (forward-sexp)
                                     (point))))
              (set-match-data (list def-beg def-end)))))
        (goto-char start)))))

(eval-and-compile
  (defconst cljm--sym-forbidden-rest-chars
    "][\";\'@\\^`~\(\)\{\}\\,\s\t\n\r"
    "A list of chars that a Clojure symbol cannot contain.
See definition of 'macros': URL `http://git.io/vRGLD'.")
  (defconst cljm--sym-forbidden-1st-chars
    (concat cljm--sym-forbidden-rest-chars "0-9:")
    "A list of chars that a Clojure symbol cannot start with.
See the for-loop: URL `http://git.io/vRGTj' lines: URL
`http://git.io/vRGIh', URL `http://git.io/vRGLE' and value
definition of 'macros': URL `http://git.io/vRGLD'.")
  (defconst cljm--sym-regexp
    (concat "[^" cljm--sym-forbidden-1st-chars
            "][^" cljm--sym-forbidden-rest-chars "]*")
    "A regexp matching a Clojure symbol or namespace alias.
Matches the rule `cljm--sym-forbidden-1st-chars' followed by
any number of matches of `cljm--sym-forbidden-rest-chars'."))

(defconst cljm-font-lock-keywords
  (eval-when-compile
    `( ;; Top-level variable definition
      (,(concat "(\\(?:clojure.core/\\)?\\("
                (regexp-opt '("def" "defonce"))
                ;; variable declarations
                "\\)\\>"
                ;; Any whitespace
                "[ \r\n\t]*"
                ;; Possibly type or metadata
                "\\(?:#?^\\(?:{[^}]*}\\|\\sw+\\)[ \r\n\t]*\\)*"
                "\\(\\sw+\\)?")
       (1 font-lock-keyword-face)
       (2 font-lock-variable-name-face nil t))
      ;; Type definition
      (,(concat "(\\(?:clojure.core/\\)?\\("
                (regexp-opt '("defstruct" "deftype" "defprotocol"
                              "defrecord"))
                ;; type declarations
                "\\)\\>"
                ;; Any whitespace
                "[ \r\n\t]*"
                ;; Possibly type or metadata
                "\\(?:#?^\\(?:{[^}]*}\\|\\sw+\\)[ \r\n\t]*\\)*"
                "\\(\\sw+\\)?")
       (1 font-lock-keyword-face)
       (2 font-lock-type-face nil t))
      ;; Function definition (anything that starts with def and is not
      ;; listed above)
      (,(concat "(\\(?:" cljm--sym-regexp "/\\)?"
                "\\(def[^ \r\n\t]*\\)"
                ;; Function declarations
                "\\>"
                ;; Any whitespace
                "[ \r\n\t]*"
                ;; Possibly type or metadata
                "\\(?:#?^\\(?:{[^}]*}\\|\\sw+\\)[ \r\n\t]*\\)*"
                (concat "\\(" cljm--sym-regexp "\\)?"))
       (1 font-lock-keyword-face)
       (2 font-lock-function-name-face nil t))
      ;; (fn name? args ...)
      (,(concat "(\\(?:clojure.core/\\)?\\(fn\\)[ \t]+"
                ;; Possibly type
                "\\(?:#?^\\sw+[ \t]*\\)?"
                ;; Possibly name
                "\\(\\sw+\\)?" )
       (1 font-lock-keyword-face)
       (2 font-lock-function-name-face nil t))
      ;; lambda arguments - %, %&, %1, %2, etc
      ("\\<%[&1-9]?" (0 font-lock-variable-name-face))
      ;; Special forms
      (,(concat
         "("
         (regexp-opt
          '("def" "do" "if" "let" "let*" "var" "fn" "fn*" "loop" "loop*"
            "recur" "throw" "try" "catch" "finally"
            "set!" "new" "."
            "monitor-enter" "monitor-exit" "quote") t)
         "\\>")
       1 font-lock-keyword-face)
      ;; Built-in binding and flow of control forms
      (,(concat
         "(\\(?:clojure.core/\\)?"
         (regexp-opt
          '("letfn" "case" "cond" "cond->" "cond->>" "condp"
            "for" "when" "when-not" "when-let" "when-first" "when-some"
            "if-let" "if-not" "if-some"
            ".." "->" "->>" "as->" "doto" "and" "or"
            "dosync" "doseq" "dotimes" "dorun" "doall"
            "ns" "in-ns"
            "with-open" "with-local-vars" "binding"
            "with-redefs" "with-redefs-fn"
            "declare") t)
         "\\>")
       1 font-lock-keyword-face)
      ;; Macros similar to let, when, and while
      (,(rx symbol-start
            (or "let" "when" "while") "-"
            (1+ (or (syntax word) (syntax symbol)))
            symbol-end)
       0 font-lock-keyword-face)
      (,(concat
         "\\<"
         (regexp-opt
          '("*1" "*2" "*3" "*agent*"
            "*allow-unresolved-vars*" "*assert*" "*clojure-version*"
            "*command-line-args*" "*compile-files*"
            "*compile-path*" "*data-readers*" "*default-data-reader-fn*"
            "*e" "*err*" "*file*" "*flush-on-newline*"
            "*in*" "*macro-meta*" "*math-context*" "*ns*" "*out*"
            "*print-dup*" "*print-length*" "*print-level*"
            "*print-meta*" "*print-readably*"
            "*read-eval*" "*source-path*"
            "*unchecked-math*"
            "*use-context-classloader*" "*warn-on-reflection*")
          t)
         "\\>")
       0 font-lock-builtin-face)
      ;; Dynamic variables - *something* or @*something*
      (,(concat "\\(?:\\<\\|/\\)@?\\(\\*" cljm--sym-regexp "\\*\\)\\>")
       1 font-lock-variable-name-face)
      ;; Global constants - nil, true, false
      (,(concat
         "\\<"
         (regexp-opt
          '("true" "false" "nil") t)
         "\\>")
       0 font-lock-constant-face)
      ;; Character literals - \1, \a, \newline, \u0000
      ("\\\\\\([[:punct:]]\\|[a-z0-9]+\\>\\)" 0 'cljm-character-face)

      ;; namespace definitions: (ns foo.bar)
      (,(concat "(\\<ns\\>[ \r\n\t]*"
                ;; Possibly metadata, shorthand and/or longhand
                "\\(?:\\^?\\(?:{[^}]+}\\|:[^ \r\n\t]+[ \r\n\t]\\)[ \r\n\t]*\\)*"
                ;; namespace
                "\\(" cljm--sym-regexp "\\)")
       (1 font-lock-type-face))

      ;; TODO dedupe the code for matching of keywords, type-hints and
      ;; unmatched symbols

      ;; keywords: {:oneword/ve/yCom|pLex.stu-ff 0}
      (,(concat "\\(:\\{1,2\\}\\)\\(" cljm--sym-regexp "?\\)\\(/\\)\\("
                cljm--sym-regexp "\\)")
       (1 'cljm-keyword-face)
       (2 font-lock-type-face)
       ;; (2 'cljm-keyword-face)
       (3 'default)
       (4 'cljm-keyword-face))
      (,(concat "\\(:\\{1,2\\}\\)\\(" cljm--sym-regexp "\\)")
       (1 'cljm-keyword-face)
       (2 'cljm-keyword-face))

      ;; type-hints: #^oneword
      (,(concat "\\(#?\\^\\)\\(" cljm--sym-regexp "?\\)\\(/\\)\\("
                cljm--sym-regexp "\\)")
       (1 'default)
       (2 font-lock-type-face)
       (3 'default)
       (4 'default))
      (,(concat "\\(#?\\^\\)\\(" cljm--sym-regexp "\\)")
       (1 'default)
       (2 font-lock-type-face))

      ;; clojure symbols not matched by the previous regexps; influences CIDER's
      ;; dynamic syntax highlighting (CDSH). See https://git.io/vxEEA:
      (,(concat "\\(" cljm--sym-regexp "?\\)\\(/\\)\\(" cljm--sym-regexp "\\)")
       (1 font-lock-type-face)
       ;; 2nd and 3th matching groups can be font-locked to `nil' or `default'.
       ;; CDSH seems to kick in only for functions and variables referenced w/o
       ;; writing their namespaces.
       (2 nil)
       (3 nil))
      (,(concat "\\(" cljm--sym-regexp "\\)")
       ;; this matching group must be font-locked to `nil' otherwise CDSH breaks.
       (1 nil))

      ;; #_ and (comment ...) macros.
      (cljm-search-comment-macro 1 font-lock-comment-face t)
      ;; Highlight `code` marks, just like `elisp'.
      (,(rx "`" (group-n 1 (optional "#'")
                         (+ (or (syntax symbol) (syntax word)))) "`")
       (1 'font-lock-constant-face prepend))
      ;; Highlight [[var]] comments
      (,(rx "[[" (group-n 1 (optional "#'")
                         (+ (or (syntax symbol) (syntax word)))) "]]")
       (1 'font-lock-constant-face prepend))
      ;; Highlight escaped characters in strings.
      (cljm-font-lock-escaped-chars 0 'bold prepend)
      ;; Highlight grouping constructs in regular expressions
      (cljm-font-lock-regexp-groups
       (1 'font-lock-regexp-grouping-construct prepend))))
  "Default expressions to highlight in Clojure mode.")

(defun cljm-font-lock-syntactic-face-function (state)
  "Find and highlight text with a Clojure-friendly syntax table.

This function is passed to `font-lock-syntactic-face-function',
which is called with a single parameter, STATE (which is, in
turn, returned by `parse-partial-sexp' at the beginning of the
highlighted region)."
  (if (nth 3 state)
      ;; This might be a (doc)string or a |...| symbol.
      (let ((startpos (nth 8 state)))
        (if (eq (char-after startpos) ?|)
            ;; This is not a string, but a |...| symbol.
            nil
          (let* ((listbeg (nth 1 state))
                 (firstsym (and listbeg
                                (save-excursion
                                  (goto-char listbeg)
                                  (and (looking-at
                                        "([ \t\n]*\\(\\(\\sw\\|\\s_\\)+\\)")
                                       (match-string 1)))))
                 (docelt (and firstsym
                              (function-get (intern-soft firstsym)
                                            lisp-doc-string-elt-property))))
            (if (and docelt
                     ;; It's a string in a form that can have a docstring.
                     ;; Check whether it's in docstring position.
                     (save-excursion
                       (when (functionp docelt)
                         (goto-char (match-end 1))
                         (setq docelt (funcall docelt)))
                       (goto-char listbeg)
                       (forward-char 1)
                       (ignore-errors
                         (while (and (> docelt 0) (< (point) startpos)
                                     (progn (forward-sexp 1) t))
                           ;; ignore metadata and type hints
                           (unless (looking-at
                                    "[ \n\t]*\\(\\^[A-Z:].+\\|\\^?{.+\\)")
                             (setq docelt (1- docelt)))))
                       (and (zerop docelt) (<= (point) startpos)
                            (progn (forward-comment (point-max)) t)
                            (= (point) (nth 8 state))))
                     ;; In a def, at last position is not a docstring
                     (not (and (string= "def" firstsym)
                               (save-excursion
                                 (goto-char startpos)
                                 (goto-char (+ startpos
                                               (length (sexp-at-point)) 2))
                                 (looking-at "[ \r\n\t]*\)")))))
                font-lock-doc-face
              font-lock-string-face))))
    font-lock-comment-face))

(defun cljm-font-lock-setup ()
  "Configures font-lock for editing Clojure code."
  (setq-local font-lock-multiline t)
  (add-to-list 'font-lock-extend-region-functions
               #'cljm-font-lock-extend-region-def t)
  (setq font-lock-defaults
        '(cljm-font-lock-keywords    ; keywords
          nil nil
          (("+-*/.<>=!?$%_&:" . "w")) ; syntax alist
          nil
          (font-lock-mark-block-function . mark-defun)
          (font-lock-syntactic-face-function
           . cljm-font-lock-syntactic-face-function))))

(defun cljm-font-lock-def-at-point (point)
  "Range between the top-most def* and the fourth element after POINT.
Note that this means that there is no guarantee of proper font
locking in def* forms that are not at top level."
  (goto-char point)
  (ignore-errors
    (beginning-of-defun))

  (let ((beg-def (point)))
    (when (and (not (= point beg-def))
               (looking-at "(def"))
      (ignore-errors
        ;; move forward as much as possible until failure (or success)
        (forward-char)
        (dotimes (_ 4)
          (forward-sexp)))
      (cons beg-def (point)))))

(defun cljm-font-lock-extend-region-def ()
  "Set region boundaries to include the first four elements of def* forms."
  (let ((changed nil))
    (let ((def (cljm-font-lock-def-at-point font-lock-beg)))
      (when def
        (cl-destructuring-bind (def-beg . def-end) def
          (when (and (< def-beg font-lock-beg)
                     (< font-lock-beg def-end))
            (setq font-lock-beg def-beg
                  changed t)))))
    (let ((def (cljm-font-lock-def-at-point font-lock-end)))
      (when def
        (cl-destructuring-bind (def-beg . def-end) def
          (when (and (< def-beg font-lock-end)
                     (< font-lock-end def-end))
            (setq font-lock-end def-end
                  changed t)))))
    changed))

(defun cljm--font-locked-as-string-p (&optional regexp)
  "Non-nil if the char before point is font-locked as a string.
If REGEXP is non-nil, also check whether current string is
preceeded by a #."
  (let ((face (get-text-property (1- (point)) 'face)))
    (and (or (and (listp face)
                  (memq 'font-lock-string-face face))
             (eq 'font-lock-string-face face))
         (or (cljm-string-start t)
             (unless regexp
               (cljm-string-start nil))))))

(defun cljm-font-lock-escaped-chars (bound)
  "Highlight \escaped chars in strings.
BOUND denotes a buffer position to limit the search."
  (let ((found nil))
    (while (and (not found)
                (re-search-forward "\\\\." bound t))

      (setq found (cljm--font-locked-as-string-p)))
    found))

(defun cljm-font-lock-regexp-groups (bound)
  "Highlight grouping constructs in regular expression.

BOUND denotes the maximum number of characters (relative to the
point) to check."
  (let ((found nil))
    (while
        (and (not found)
             (re-search-forward
              (eval-when-compile
                (concat
                 ;; A group may start using several alternatives:
                 "\\(\\(?:"
                 ;; 1. (? special groups
                 "(\\?\\(?:"
                 ;; a) non-capturing group (?:X)
                 ;; b) independent non-capturing group (?>X)
                 ;; c) zero-width positive lookahead (?=X)
                 ;; d) zero-width negative lookahead (?!X)
                 "[:=!>]\\|"
                 ;; e) zero-width positive lookbehind (?<=X)
                 ;; f) zero-width negative lookbehind (?<!X)
                 "<[=!]\\|"
                 ;; g) named capturing group (?<name>X)
                 "<[[:alnum:]]+>"
                 "\\)\\|" ;; end of special groups
                 ;; 2. normal capturing groups (
                 ;; 3. we also highlight alternative
                 ;; separarators |, and closing parens )
                 "[|()]"
                 "\\)\\)"))
              bound t))
      (setq found (cljm--font-locked-as-string-p 'regexp)))
    found))

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

;;; Vertical alignment
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

(defun cljm--search-whitespace-after-next-sexp (&optional bound _noerror)
  "Move point after all whitespace after the next sexp.

Set the match data group 1 to be this region of whitespace and
return point.

BOUND is bounds the whitespace search."
  (unwind-protect
      (ignore-errors
        (cljm-forward-logical-sexp 1)
        (search-forward-regexp "\\([,\s\t]*\\)" bound)
        (pcase (syntax-after (point))
          ;; End-of-line, try again on next line.
          (`(12) (cljm--search-whitespace-after-next-sexp bound))
          ;; Closing paren, stop here.
          (`(5 . ,_) nil)
          ;; Anything else is something to align.
          (_ (point))))
    (when (and bound (> (point) bound))
      (goto-char bound))))

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

;;; Indentation
(defun cljm-indent-region (beg end)
  "Like `indent-region', but also maybe align forms.
Forms between BEG and END are aligned according to
`cljm-align-forms-automatically'."
  (prog1 (let ((indent-region-function nil))
           (indent-region beg end))
    (when cljm-align-forms-automatically
      (condition-case nil
          (cljm-align beg end)
        (scan-error nil)))))

(defun cljm-indent-line ()
  "Indent current line as Clojure code."
  (if (cljm-in-docstring-p)
      (save-excursion
        (beginning-of-line)
        (when (and (looking-at "^\\s-*")
                   (<= (string-width (match-string-no-properties 0))
                       (string-width (cljm-docstring-fill-prefix))))
          (replace-match (cljm-docstring-fill-prefix))))
    (lisp-indent-line)))

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

(defvar cljm--current-backtracking-depth 0)

(defun cljm--find-indent-spec-backtracking ()
  "Return the indent sexp that applies to the sexp at point.
Implementation function for `cljm--find-indent-spec'."
  (when (and (>= cljm-max-backtracking cljm--current-backtracking-depth)
             (not (looking-at "^")))
    (let ((cljm--current-backtracking-depth
           (1+ cljm--current-backtracking-depth))
          (pos 0))
      ;; Count how far we are from the start of the sexp.
      (while (ignore-errors (cljm-backward-logical-sexp 1)
                            (not (or (bobp)
                                     (eq (char-before) ?\n))))
        (cl-incf pos))
      (let* ((function (thing-at-point 'symbol))
             (method (or (when function ;; Is there a spec here?
                           (cljm--get-indent-method function))
                         (ignore-errors
                           ;; Otherwise look higher up.
                           (pcase (syntax-ppss)
                             (`(,(pred (< 0)) ,start . ,_)
                              (goto-char start)
                              (cljm--find-indent-spec-backtracking)))))))
        (when (numberp method)
          (setq method (list method)))
        (pcase method
          ((pred functionp)
           (when (= pos 0)
             method))
          ((pred sequencep)
           (pcase (length method)
             (`0 nil)
             (`1 (let ((head (elt method 0)))
                   (when (or (= pos 0) (sequencep head))
                     head)))
             (l (if (>= pos l)
                    (elt method (1- l))
                  (elt method pos)))))
          ((or `defun `:defn)
           (when (= pos 0)
             :defn))
          (_
           (message "Invalid indent spec for `%s': %s" function method)
           nil))))))

(defun cljm--find-indent-spec ()
  "Return the indent spec that applies to the current sexp.
If `cljm-use-backtracking-indent' is non-nil, also do
backtracking up to a higher-level sexp in order to find the
spec."
  (if cljm-use-backtracking-indent
      (save-excursion
        (cljm--find-indent-spec-backtracking))
    (let ((function (thing-at-point 'symbol)))
      (cljm--get-indent-method function))))

(defun cljm--keyword-to-symbol (keyword)
  "Convert KEYWORD to symbol."
  (intern (substring (symbol-name keyword) 1)))

(defun cljm--normal-indent (last-sexp indent-mode)
  "Return the normal indentation column for a sexp.
Point should be after the open paren of the _enclosing_ sexp, and
LAST-SEXP is the start of the previous sexp (immediately before
the sexp being indented).  INDENT-MODE is any of the values
accepted by `cljm-indent-style'."
  (goto-char last-sexp)
  (forward-sexp 1)
  (cljm-backward-logical-sexp 1)
  (let ((last-sexp-start nil))
    (if (ignore-errors
          ;; `backward-sexp' until we reach the start of a sexp that is the
          ;; first of its line (the start of the enclosing sexp).
          (while (string-match
                  "[^[:blank:]]"
                  (buffer-substring (line-beginning-position) (point)))
            (setq last-sexp-start (prog1 (point)
                                    (forward-sexp -1))))
          t)
        ;; Here we have found an arg before the arg we're indenting which is at
        ;; the start of a line. Every mode simply aligns on this case.
        (current-column)
      ;; Here we have reached the start of the enclosing sexp (point is now at
      ;; the function name), so the behaviour depends on INDENT-MODE and on
      ;; whether there's also an argument on this line (case A or B).
      (let ((indent-mode (if (keywordp indent-mode)
                             ;; needed for backwards compatibility as
                             ;; pre clojure-mode 5.10 indent-mode was a keyword
                             (cljm--keyword-to-symbol indent-mode)
                           indent-mode))
            (case-a ; The meaning of case-a is explained in `cljm-indent-style'.
             (and last-sexp-start
                  (< last-sexp-start (line-end-position)))))
        (cond
         ((eq indent-mode 'always-indent)
          (+ (current-column) lisp-body-indent -1))
         ;; There's an arg after the function name, so align with it.
         (case-a (goto-char last-sexp-start)
                 (current-column))
         ;; Not same line.
         ((eq indent-mode 'align-arguments)
          (+ (current-column) lisp-body-indent -1))
         ;; Finally, just align with the function name.
         (t (current-column)))))))

(defun cljm--not-function-form-p ()
  "Non-nil if form at point doesn't represent a function call."
  (or (member (char-after) '(?\[ ?\{))
      (save-excursion ;; Catch #?@ (:cljs ...)
        (skip-chars-backward "\r\n[:blank:]")
        (when (eq (char-before) ?@)
          (forward-char -1))
        (and (eq (char-before) ?\?)
             (eq (char-before (1- (point))) ?\#)))
      ;; Car of form is not a symbol.
      (not (looking-at ".\\(?:\\sw\\|\\s_\\)"))))

;; Check the general context, and provide indentation for data structures and
;; special macros. If current form is a function (or non-special macro),
;; delegate indentation to `cljm--normal-indent'.
(defun cljm-indent-function (indent-point state)
  "When indenting a line within a function call, indent properly.

INDENT-POINT is the position where the user typed TAB, or equivalent.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Clojure function with a
non-nil property `cljm-indent-function', that specifies how to do
the indentation.

The property value can be

- `defun', meaning indent `defun'-style;
- an integer N, meaning indent the first N arguments specially
  like ordinary function arguments and then indent any further
  arguments like a body;
- a function to call just as this function was called.
  If that function returns nil, that means it doesn't specify
  the indentation.
- a list, which is used by `cljm-backtracking-indent'.

This function also returns nil meaning don't specify the indentation."
  ;; Goto to the open-paren.
  (goto-char (elt state 1))
  ;; Maps, sets, vectors and reader conditionals.
  (if (cljm--not-function-form-p)
      (1+ (current-column))
    ;; Function or macro call.
    (forward-char 1)
    (let ((method (cljm--find-indent-spec))
          (last-sexp calculate-lisp-indent-last-sexp)
          (containing-form-column (1- (current-column))))
      (pcase method
        ((or (pred integerp) `(,method))
         (let ((pos -1))
           (condition-case nil
               (while (and (<= (point) indent-point)
                           (not (eobp)))
                 (cljm-forward-logical-sexp 1)
                 (cl-incf pos))
             ;; If indent-point is _after_ the last sexp in the
             ;; current sexp, we detect that by catching the
             ;; `scan-error'. In that case, we should return the
             ;; indentation as if there were an extra sexp at point.
             (scan-error (cl-incf pos)))
           (cond
            ;; The first non-special arg. Rigidly reduce indentation.
            ((= pos (1+ method))
             (+ lisp-body-indent containing-form-column))
            ;; Further non-special args, align with the arg above.
            ((> pos (1+ method))
             (cljm--normal-indent last-sexp 'always-align))
            ;; Special arg. Rigidly indent with a large indentation.
            (t
             (+ (* 2 lisp-body-indent) containing-form-column)))))
        (`:defn
         (+ lisp-body-indent containing-form-column))
        ((pred functionp)
         (funcall method indent-point state))
        ;; No indent spec, do the default.
        (`nil
         (let ((function (thing-at-point 'symbol)))
           (cond
            ;; Preserve useful alignment of :require (+ friends) in `ns' forms.
            ((and function (string-match "^:" function))
             (cljm--normal-indent last-sexp 'always-align))
            ;; This should be identical to the :defn above.
            ((and function
                  (string-match "\\`\\(?:\\S +/\\)?\\(def[a-z]*\\|with-\\)"
                                function)
                  (not (string-match "\\`default" (match-string 1 function))))
             (+ lisp-body-indent containing-form-column))
            ;; Finally, nothing special here, just respect the user's
            ;; preference.
            (t (cljm--normal-indent last-sexp cljm-indent-style)))))))))

;;; Setting indentation
(defun put-clojure-indent (sym indent)
  "Instruct `cljm-indent-function' to indent the body of SYM by INDENT."
  (put sym 'cljm-indent-function indent))

(defmacro define-clojure-indent (&rest kvs)
  "Call `put-clojure-indent' on a series, KVS."
  `(progn
     ,@(mapcar (lambda (x) `(put-clojure-indent
                             (quote ,(car x)) ,(cadr x)))
               kvs)))

(defun add-custom-clojure-indents (name value)
  "Allow `cljm-defun-indents' to indent user-specified macros.

Requires the macro's NAME and a VALUE."
  (custom-set-default name value)
  (mapcar (lambda (x)
            (put-clojure-indent x 'defun))
          value))

(defcustom cljm-defun-indents nil
  "List of additional symbols with defun-style indentation in Clojure.

You can use this to let Emacs indent your own macros the same way
that it indents built-in macros like with-open.  This variable
only works when set via the customize interface (`setq' won't
work).  To set it from Lisp code, use
     (put-clojure-indent \\='some-symbol :defn)."
  :type '(repeat symbol)
  :set 'add-custom-clojure-indents)

(define-clojure-indent
  ;; built-ins
  (ns 1)
  (fn :defn)
  (def :defn)
  (defn :defn)
  (bound-fn :defn)
  (if 1)
  (if-not 1)
  (case 1)
  (cond 0)
  (condp 2)
  (cond-> 1)
  (cond->> 1)
  (when 1)
  (while 1)
  (when-not 1)
  (when-first 1)
  (do 0)
  (delay 0)
  (future 0)
  (comment 0)
  (doto 1)
  (locking 1)
  (proxy '(2 nil nil (:defn)))
  (as-> 2)
  (fdef 1)

  (reify '(:defn (1)))
  (deftype '(2 nil nil (:defn)))
  (defrecord '(2 nil nil (:defn)))
  (defprotocol '(1 (:defn)))
  (definterface '(1 (:defn)))
  (extend 1)
  (extend-protocol '(1 :defn))
  (extend-type '(1 :defn))
  ;; specify and specify! are from ClojureScript
  (specify '(1 :defn))
  (specify! '(1 :defn))
  (try 0)
  (catch 2)
  (finally 0)

  ;; binding forms
  (let 1)
  (letfn '(1 ((:defn)) nil))
  (binding 1)
  (loop 1)
  (for 1)
  (doseq 1)
  (dotimes 1)
  (when-let 1)
  (if-let 1)
  (when-some 1)
  (if-some 1)
  (this-as 1) ; ClojureScript

  (defmethod :defn)

  ;; clojure.test
  (testing 1)
  (deftest :defn)
  (are 2)
  (use-fixtures :defn)

  ;; core.logic
  (run :defn)
  (run* :defn)
  (fresh :defn)

  ;; core.async
  (alt! 0)
  (alt!! 0)
  (go 0)
  (go-loop 1)
  (thread 0))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Better docstring filling for cljm-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cljm-string-start (&optional regex)
  "Return the position of the \" that begins the string at point.
If REGEX is non-nil, return the position of the # that begins the
regex at point.  If point is not inside a string or regex, return
nil."
  (when (nth 3 (syntax-ppss)) ;; Are we really in a string?
    (save-excursion
      (save-match-data
        ;; Find a quote that appears immediately after whitespace,
        ;; beginning of line, hash, or an open paren, brace, or bracket
        (re-search-backward "\\(\\s-\\|^\\|#\\|(\\|\\[\\|{\\)\\(\"\\)")
        (let ((beg (match-beginning 2)))
          (when beg
            (if regex
                (and (char-before beg) (eq ?# (char-before beg)) (1- beg))
              (when (not (eq ?# (char-before beg)))
                beg))))))))



(defcustom cljm-cache-project-dir t
  "Whether to cache the results of `cljm-project-dir'."
  :type 'boolean
  :safe #'booleanp
  :package-version '(cljm-mode . "5.8.0"))

(defvar-local cljm-cached-project-dir nil
  "A project dir cache used to speed up related operations.")

(defun cljm-project-dir (&optional dir-name)
  "Return the absolute path to the project's root directory.

Call is delegated down to `cljm-project-root-function' with
optional DIR-NAME as argument.

When `cljm-cache-project-dir' is t the results of the command
are cached in a buffer local variable (`cljm-cached-project-dir')."
  (let ((project-dir (or cljm-cached-project-dir
                         (funcall cljm-project-root-function dir-name))))
    (when (and cljm-cache-project-dir
               (derived-mode-p 'cljm-mode)
               (not cljm-cached-project-dir))
      (setq cljm-cached-project-dir project-dir))
    project-dir))

(defun cljm-current-project (&optional dir-name)
  "Return the current project as a cons cell usable by project.el.

Call is delegated down to `cljm-project-dir' with
optional DIR-NAME as argument."
  (let ((project-dir (cljm-project-dir dir-name)))
    (if project-dir
        (cons 'clojure project-dir)
      nil)))

(defun cljm-project-root-path (&optional dir-name)
  "Return the absolute path to the project's root directory.

Use `default-directory' if DIR-NAME is nil.
Return nil if not inside a project."
  (let* ((dir-name (or dir-name default-directory))
         (choices (delq nil
                        (mapcar (lambda (fname)
                                  (locate-dominating-file dir-name fname))
                                cljm-build-tool-files))))
    (when (> (length choices) 0)
      (car (sort choices #'file-in-directory-p)))))

;; project.el integration
(cl-defmethod project-roots ((project (head clojure)))
  (list (cdr project)))



;;; Sexp navigation

(defun cljm--looking-at-non-logical-sexp ()
  "Return non-nil if text after point is \"non-logical\" sexp.
\"Non-logical\" sexp are ^metadata and #reader.macros."
  (comment-normalize-vars)
  (comment-forward (point-max))
  (looking-at-p "\\^\\|#:?:?[[:alpha:]]"))

(defun cljm-forward-logical-sexp (&optional n)
  "Move forward N logical sexps.
This will skip over sexps that don't represent objects, so that ^hints and
#reader.macros are considered part of the following sexp."
  (interactive "p")
  (unless n (setq n 1))
  (if (< n 0)
      (cljm-backward-logical-sexp (- n))
    (let ((forward-sexp-function nil))
      (while (> n 0)
        (while (cljm--looking-at-non-logical-sexp)
          (forward-sexp 1))
        ;; The actual sexp
        (forward-sexp 1)
        (skip-chars-forward ",")
        (setq n (1- n))))))

(defun cljm-backward-logical-sexp (&optional n)
  "Move backward N logical sexps.
This will skip over sexps that don't represent objects, so that ^hints and
#reader.macros are considered part of the following sexp."
  (interactive "p")
  (unless n (setq n 1))
  (if (< n 0)
      (cljm-forward-logical-sexp (- n))
    (let ((forward-sexp-function nil))
      (while (> n 0)
        ;; The actual sexp
        (backward-sexp 1)
        ;; Non-logical sexps.
        (while (and (not (bobp))
                    (ignore-errors
                      (save-excursion
                        (backward-sexp 1)
                        (cljm--looking-at-non-logical-sexp))))
          (backward-sexp 1))
        (setq n (1- n))))))

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

(defun cljm--in-string-p ()
  "Check whether the point is currently in a string."
  (nth 3 (syntax-ppss)))

;;; let related stuff

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


;;; ClojureScript
(defconst cljms-font-lock-keywords
  (eval-when-compile
    `(;; ClojureScript built-ins
      (,(concat "(\\(?:\.*/\\)?"
                (regexp-opt '("js-obj" "js-delete" "clj->js" "js->clj"))
                "\\>")
       0 font-lock-builtin-face)))
  "Additional font-locking for `cljms-mode'.")

;;;###autoload
(define-derived-mode cljms-mode cljm-mode "ClojureScript"
  "Major mode for editing ClojureScript code.

\\{clojurescript-mode-map}"
  (font-lock-add-keywords nil cljms-font-lock-keywords))

;;;###autoload
(define-derived-mode cljmc-mode cljm-mode "Clojure with Reader Conditionals"
  "Major mode for editing Clojure code with reader conditionals.

\\{cljmc-mode-map}")

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist
               '("\\.\\(clj\\|dtm\\|edn\\)\\'" . cljm-mode))
  (add-to-list 'auto-mode-alist '("\\.cljc\\'" . cljmc-mode))
  (add-to-list 'auto-mode-alist '("\\.cljs\\'" . cljms-mode))
  ;; boot build scripts are Clojure source files
  (add-to-list 'auto-mode-alist '("\\(?:build\\|profile\\)\\.boot\\'" .
                                  cljm-mode)))

(provide 'cljm-mode)

;; Local Variables:
;; coding: utf-8
;; End:

;;; cljm.el ends here
