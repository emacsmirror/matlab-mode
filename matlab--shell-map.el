;;; matlab--shell-map.el --- -*- lexical-binding: t -*-

;;; Commentary:
;; Keymaps used in both matlab-ts-mode and matlab-mode for matlab-shell.
;;

;;; Code:

(defvar matlab--shell-help-map
  (let ((km (make-sparse-keymap)))
    (define-key km "r" 'matlab-shell-run-command)
    (define-key km "f" 'matlab-shell-describe-command)
    (define-key km "a" 'matlab-shell-apropos)
    (define-key km "v" 'matlab-shell-describe-variable)
    km)
  "Help key map for `matlab-ts-mode' / `matlab-mode' and `matlab-shell-mode'.")

(provide 'matlab--shell-map)
;;; matlab--shell-map.el ends here

