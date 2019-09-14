;;; cljm-mode.el --- Major mode for Clojure code -*- lexical-binding: t; -*-

;; Stripped down and broken up clojure-mode.el

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
;; misc functions supporting above code

;;; Code:


(eval-when-compile
  (defvar font-lock-beg)
  (defvar font-lock-end))

(require 'cljm-align)
(require 'cljm-docstring)
(require 'cljm-fill)
(require 'cljm-indent)
(require 'cljm-motion)
(require 'cljm-paredit)
(require 'cljm-project)
(require 'cljm-refs)
(require 'cljm-verify)

(require 'cl-lib)
(require 'imenu)
(require 'newcomment)
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

;;; cljm-mode.el ends here
