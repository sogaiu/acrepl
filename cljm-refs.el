;;; cljm-refs.el --- cljm clojure references -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

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

(provide 'cljm-refs)

;;; cljm-refs.el ends here
