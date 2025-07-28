;;; matlab--shared.el --- -*- lexical-binding: t -*-

;;; Commentary:
;; Items shared by matlab-ts-mode, matlab-mode, or matlab-shell.
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

(defun matlab-justify-line ()
  "Delete space on end of line and justify."
  (interactive)
  (save-excursion
    (end-of-line)
    (delete-horizontal-space)
    (justify-current-line)))

(provide 'matlab--shared)
;;; matlab--shared.el ends here
