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

;; Support debug mode and read only toggling.
(defvar gud-matlab-debug-active nil)
(declare-function matlab-shell-gud-minor-mode "matlab-shell-gud")

(defun matlab-toggle-read-only (&optional arg)
  "Toggle read-only bit in MATLAB mode.
This looks to see if we are currently debugging, and if so re-enable our
debugging feature.  Optional argument ARG is ignored.  ARG is present
because we are replacing `read-only-mode' key defintion with this
function."

  (interactive "P")
  (ignore arg)
  (if (and (featurep 'matlab-shell-gud)
           gud-matlab-debug-active)
      ;; The debugging is active, just re-enable debugging read-only-mode
      (matlab-shell-gud-minor-mode 1)
    ;; Else - it is not - probably doing something else.
    (call-interactively 'read-only-mode)))

(provide 'matlab--shared)
;;; matlab--shared.el ends here
