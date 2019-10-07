;;; acrepl-interaction.el --- interaction -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;; Requirements

(require 'acrepl-ascertain)
(require 'acrepl-load)
(require 'acrepl-send)
(require 'acrepl-tap)

(defvar acrepl-interaction-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-e" 'acrepl-send-expr-at-point)
    (define-key map "\C-c\C-l" 'acrepl-load-buffer-file)
    (define-key map "\C-c\C-r" 'acrepl-send-region)
    (define-key map "\C-c\C-x" 'acrepl-set-current-conn)
    (define-key map "\C-c\C-y" 'acrepl)
    (define-key map "\C-c\C-z" 'acrepl-switch-to-repl)
    ;; XXX: may be not so important?
    (define-key map "\C-c\C-a" 'acrepl-send-ascertained-region)
    (define-key map "\C-c\C-b" 'acrepl-send-buffer)
    (define-key map "\C-c\C-i" 'acrepl-load-file)
    (define-key map "\C-c\C-t" 'acrepl-tap-expr-at-point)
    (easy-menu-define acrepl-interaction-mode-menu map
      "A Clojure REPL Interaction Mode Menu"
      '("ACRepl"
        ["Send expression at point" acrepl-send-expr-at-point t]
        ["Send region" acrepl-send-region t]
        "--"
        ["Load buffer file" acrepl-load-buffer-file t]
        "--"
        ["New Connection" acrepl t]
        ["Set Connection" acrepl-set-current-conn t]
        ["Reconnect" acrepl-reconnect t]
        "--"
        ["Switch to REPL" acrepl-switch-to-repl t]
        "--"
        ("Extras"
         ["Send ascertained region" acrepl-send-ascertained-region t]
         ["Send buffer" acrepl-send-buffer t]
         "--"
         ["tap> expression at point" acrepl-tap-expr-at-point t]
         ["tap> region" acrepl-tap-region t]
         "--"
         ["Load file" acrepl-load-file t])))
    map)
  "Keymap for acrepl interaction mode.")

;;;###autoload
(define-minor-mode acrepl-interaction-mode
  "Minor mode for acrepl interaction from a Clojure buffer.
The following keys are available in `acrepl-interaction-mode`:
\\{acrepl-interaction-mode}"
  nil " acrepl" acrepl-interaction-mode-map)

(provide 'acrepl-interaction)

;;; acrepl-interaction.el ends here
