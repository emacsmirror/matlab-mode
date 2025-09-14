;;; matlab--shared.el --- -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.
;;
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
;; along with this program.  If not, see <http://www.gnu.org/licenses>.

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
because we are replacing `read-only-mode' key definition with this
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

;; LocalWords:  keymap defun gud featurep
