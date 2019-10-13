;;; acrepl-state.el --- state -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;; Requirements

(require 'subr-x)

;; conn == conn-name == repl-buffer-name
;;   [1]*ind/core.clj*[23579]
;; path
;;   /home/user/src/index/src/index/core.clj
(defun acrepl-empty-context ()
  "Create an empty context."
  (list
   (cons :conn-counter 0)
   (cons :conn-to-endpoint
         (make-hash-table :test #'equal))
   (cons :path-to-conn
         (make-hash-table :test #'equal))
   (cons :conn-to-paths
         (make-hash-table :test #'equal))))

(defvar acrepl-context
  (acrepl-empty-context)
  "Holds most of the state of acrepl.")

;;; XXX: not too happy about the naming of some / all of the functions below

;;; XXX: any nice way to pretty print elisp data structures?

;;; XXX: revisit use of `error' all throughout below

(defun acrepl-get-conn-counter ()
  "Get number of attempted connections."
  (let ((conn-counter (alist-get :conn-counter acrepl-context)))
    (when (not (numberp conn-counter))
      (error "Expected number, but found: %S" (type-of conn-counter)))
    conn-counter))

(defun acrepl-get-conn-to-endpoint ()
  "Get the connection to endpoint table."
  (let ((conn-to-endpoint (alist-get :conn-to-endpoint acrepl-context)))
    (when (not (hash-table-p conn-to-endpoint))
      (error "Expected hash table, but found: %S" (type-of conn-to-endpoint)))
    conn-to-endpoint))

(defun acrepl-get-path-to-conn ()
  "Get the path to connection table."
  (let ((path-to-conn (alist-get :path-to-conn acrepl-context)))
    (when (not (hash-table-p path-to-conn))
      (error "Expected hash table, but found: %S" (type-of path-to-conn)))
    path-to-conn))

(defun acrepl-get-conn-to-paths ()
  "Get the connection to paths table."
  (let ((conn-to-paths (alist-get :conn-to-paths acrepl-context)))
    (when (not (hash-table-p conn-to-paths))
      (error "Expected hash table, but found: %S" (type-of conn-to-paths)))
    conn-to-paths))

;;;

(defun acrepl-inc-conn-counter! ()
  "Increment the connection counter."
  (let ((acrepl-conn-counter (acrepl-get-conn-counter)))
    (setf (cdr (assoc :conn-counter acrepl-context))
          (1+ acrepl-conn-counter))))

(defun acrepl-get-host (endpoint)
  "Get host given ENDPOINT."
  (car (split-string endpoint ":")))

(defun acrepl-get-port (endpoint)
  "Get port given ENDPOINT."
  (string-to-number (cadr (split-string endpoint ":"))))

(defun acrepl-make-endpoint (host port)
  "Create endpoint from HOST and PORT."
  (format "%s:%s" host port))

(defun acrepl-get-endpoint (conn)
  "Get network endpoint associated with CONN."
  (let* ((conn-to-endpoint (acrepl-get-conn-to-endpoint))
         (endpoint (gethash conn conn-to-endpoint)))
    (when (not endpoint)
      (error "No endpoint found for connection: %S" conn))
    endpoint))

(defun acrepl-set-endpoint! (conn host port)
  "Set network endpoint HOST, PORT for CONN."
  (let* ((conn-to-endpoint (acrepl-get-conn-to-endpoint)))
    (puthash conn (acrepl-make-endpoint host port) conn-to-endpoint)))

(defun acrepl-remove-endpoint! (conn)
  "Remove network endpoint HOST, PORT for CONN."
  (let* ((conn-to-endpoint (acrepl-get-conn-to-endpoint)))
    (remhash conn conn-to-endpoint)))

(defun acrepl-get-path (code-buffer-or-path)
  "Get truename path associated with CODE-BUFFER-OR-PATH."
  (let ((path (cond ((stringp code-buffer-or-path)
                     code-buffer-or-path)
                    ((bufferp code-buffer-or-path)
                     (buffer-file-name code-buffer-or-path))
                    (t nil))))
    (when (not path)
      (error "Failed to determine path for: %S" code-buffer-or-path))
    (when (not (file-exists-p path))
      (error "File doesn't appear to exist: %S" path))
    (file-truename path)))

(defun acrepl-get-conn (code-buffer-or-path)
  "Get conn for CODE-BUFFER-OR-PATH."
  (let* ((path (acrepl-get-path code-buffer-or-path))
         (path-to-conn (acrepl-get-path-to-conn))
         (conn (gethash path path-to-conn)))
    conn))

(defun acrepl-set-conn! (code-buffer-or-path conn)
  "Set CONN for CODE-BUFFER-OR-PATH."
  (let* ((path (acrepl-get-path code-buffer-or-path))
         (path-to-conn (acrepl-get-path-to-conn)))
    (puthash path conn path-to-conn)))

(defun acrepl-remove-conn! (code-buffer-or-path)
  "Remove connection for CODE-BUFFER-OR-PATH."
  (let* ((path (acrepl-get-path code-buffer-or-path))
         (path-to-conn (acrepl-get-path-to-conn)))
    (remhash path path-to-conn)))

(defun acrepl-get-repl-buffer (code-buffer-or-path)
  "Get repl-buffer for CODE-BUFFER-OR-PATH."
  (let ((conn (acrepl-get-conn code-buffer-or-path)))
    (if (not conn)
        (with-temp-message
            (format "No connection found for path: %S" code-buffer-or-path)
          nil)
      (let ((repl-buffer (get-buffer conn)))
        (when (not repl-buffer)
          (error "Failed to get repl-buffer for conn: %S" code-buffer-or-path))
        repl-buffer))))

;; (defun acrepl-get-proj-dir (code-buffer-or-path)
;;   ""
;;   )

;; (defun acrepl-get-proj-type (code-buffer-or-path)
;;   ""
;;   )

(defun acrepl-get-conn-users (conn)
  "Get paths that are using CONN."
  (let* ((conn-to-paths (acrepl-get-conn-to-paths))
         (paths (gethash conn conn-to-paths)))
    paths))

(defun acrepl-add-conn-user! (conn path)
  "Add PATH as a user of CONN."
  (let* ((conn-to-paths (acrepl-get-conn-to-paths))
         (paths (gethash conn conn-to-paths))
         (tpath (file-truename path)))
    (when (not (member tpath paths))
      (puthash conn (cons tpath paths) conn-to-paths))))

(defun acrepl-remove-conn-user! (conn path)
  "Remove PATH if it is a user of CONN."
  (let* ((conn-to-paths (acrepl-get-conn-to-paths))
         (paths (gethash conn conn-to-paths))
         (tpath (file-truename path)))
    (when (member tpath paths)
      (puthash conn (remove tpath paths) conn-to-paths))))

(defun acrepl-clear-conn-users! (conn)
  "Remove paths using CONN."
  (let* ((conn-to-paths (acrepl-get-conn-to-paths)))
    (remhash conn conn-to-paths)))

;;;

(defun acrepl-make-repl-buffer-name (path port)
  "Create a unique-ish repl buffer name using PATH, PORT and other info."
  ;; sample: src/augistints/core.cljc -> (1) augistints (2) core.cljc
  (let ((re-parent-and-file (rx (0+ anything)
                                "/"
                                (group-n 1 (1+ (not (any "/")))) ; parent
                                "/"
                                (group-n 2 (1+ (not (any "/")))) ; file
                                eol)))
    (when (not (string-match re-parent-and-file path))
      (error "Failed to parse: %s" path))
    ;; XXX: checking?
    (let ((dir-name (match-string 1 path))
          (file-name (match-string 2 path))
          (acrepl-conn-counter (acrepl-inc-conn-counter!)))
      (format "[%s]*%s/%s*[%s]"
        acrepl-conn-counter
        (substring dir-name 0 (min 3 (length dir-name)))
        file-name
        port))))

(defun acrepl-repl-buffer-name? (buffer-name)
  "Check if `acrepl-make-repl-buffer-name' could have made BUFFER-NAME."
  ;; sample: [1]*aug/core.cljc*[23579]
  (let ((re-rbn (rx bol
                    "[" (1+ digit) "]" ; conn counter
                    "*" (1+ (not (any "/"))) "/" (1+ (not (any "/"))) "*"
                    "[" (1+ digit) "]" ; port
                    eol)))
  (string-match re-rbn buffer-name)))

(defun acrepl-conn-names ()
  "Return list of connection names."
  (let ((names '()))
    (maphash (lambda (k _)
               (push k names))
             (acrepl-get-conn-to-endpoint))
    names))

(defun acrepl-update-conn-path! (conn-name code-path)
  "Update state for CONN-NAME and CODE-PATH."
  (when-let ((old-conn-name (acrepl-get-conn code-path)))
    (acrepl-remove-conn-user! old-conn-name code-path))
  (acrepl-add-conn-user! conn-name code-path)
  (acrepl-set-conn! code-path conn-name))

(provide 'acrepl-state)

;;; acrepl-state.el ends here
