;;; matlab--shell-bridge.el --- -*- lexical-binding: t -*-

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
;; Utilities to bridge between the older matlab-mode and matlab-ts-mode
;;

;;; Code:


(defun matlab--is-matlab-ts-mode-active ()
  "Return t if `matlab-ts-mode' is active.
Is `matlab-ts-mode' active per
  (add-to-list \\='major-mode-remap-alist \\='(matlab-mode . matlab-ts-mode))
If so, require `matlab-ts-mode' else require `matlab-mode'?"
  (cond
   ((and (boundp 'major-mode-remap-alist)
         (rassoc 'matlab-ts-mode major-mode-remap-alist)) ;; major-mode-remap-alist came w/ Emacs 29
    (require 'matlab-ts-mode)
    t)
   (t
    (require 'matlab-mode)
    nil)))

(defvar matlab-ts-mode--syntax-table)
(defvar matlab-mode-syntax-table)

(defun matlab--shell-get-syntax-table ()
  "Get either matlab-ts-mode or matlab-mode syntax table.
If using `matlab-ts-mode' per
  (add-to-list \\='major-mode-remap-alist \\='(matlab-mode . matlab-ts-mode))
use `matlab-ts-mode--syntax-table' else use
`matlab-mode-syntax-table'.  The tables are the same with respect
to strings and comments."
  (if (matlab--is-matlab-ts-mode-active)
      matlab-ts-mode--syntax-table
    matlab-mode-syntax-table))

(defvar matlab-ts-mode-map)
(defvar matlab-mode-map)

(defun matlab--get-matlab-map ()
  "Return either `matlab-mode-map' or `matlab-ts-mode-map'."
  (if (matlab--is-matlab-ts-mode-active)
      matlab-ts-mode-map
    matlab-mode-map))

(declare-function matlab-scan-beginning-of-command "matlab-mode")
(declare-function matlab-scan-end-of-command "matlab-mode")

(declare-function matlab-ts-mode-beginning-of-command "matlab-ts-mode")
(declare-function matlab-ts-mode-end-of-command "matlab-ts-mode")

(defun matlab--get-command-at-point-to-run ()
  "Based on the major-mode get the MATLAB command to run in `matlab-shell'."
  (save-excursion
    (pcase major-mode
      ('matlab-ts-mode
       (require 'matlab-ts-mode)
       (let* ((start-point (save-excursion
                             (matlab-ts-mode-beginning-of-command)))
              (end-point   (when start-point
                             (save-excursion
                               (matlab-ts-mode-end-of-command)))))
         (if start-point
             (buffer-substring-no-properties start-point end-point)
           "")))
      ('matlab-mode
       (require 'matlab-mode)
       (save-excursion
         (buffer-substring-no-properties
          (matlab-scan-beginning-of-command)
          (matlab-scan-end-of-command))))
      (_
       (error "This cannot be run in %s" (symbol-name major-mode))))))

(defun matlab--function-called-at-point ()
  "Return a string representing the function called nearby point."
  (save-excursion
    (beginning-of-line)
    (cond ((looking-at "\\s-*\\([a-zA-Z]\\w+\\)[^=][^=]")
           (match-string 1))
          ((and (re-search-forward "=" (line-end-position) t)
                (looking-at "\\s-*\\([a-zA-Z]\\w+\\)\\s-*[^=]"))
           (match-string 1))
          (t nil))))

(provide 'matlab--shell-bridge)
;;; matlab--shell-bridge.el ends here

;; LocalWords:  defun alist rassoc pcase boundp
