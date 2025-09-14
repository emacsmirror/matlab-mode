;;; eval-buffer-fully.el --- -*- lexical-binding: t -*-

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
;;
;; Development utility useful after modifying a Lisp buffer
;;
;;    M-x eval-buffer-fully

;;; Code:

(defun eval-buffer-fully ()
  "Try to fully evaluate the buffer.
Evaluate a Lisp buffer by running \\[eval-buffer].  If the buffer has
already been evaluated, `eval-buffer' will only re-evaluate `defun'
definitions.  Therefore, this function will search for other definitions
on their own line and run \\[eval-defun] on them.  This will miss some
items such as use of macros."
  (interactive)
  (save-excursion
    (eval-buffer)
    (goto-char (point-min))
    (while (re-search-forward
            (rx (seq bol
                     (0+ (in " " "\t"))
                     "("
                     (or "defvar"
                         "defvar-local"
                         "defface"
                         "defmacro"
                         "easy-menu-define"
                         "defvar-keymap"
                         "define-derived-mode")
                     (1+ (in " " "\t" "\r" "\n"))
                     (not (in " " "\t" "\r" "\n"))
                     ))
            nil t)
      (eval-defun nil))))

(provide 'eval-buffer-fully)
;;; eval-buffer-fully.el ends here

;; LocalWords:  defun bol defface defmacro keymap
