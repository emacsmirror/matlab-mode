;;; test-matlab-ts-mode-font-lock.el --- -*- lexical-binding: t -*-
;;
;; Copyright 2025 Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses/.

;;; Commentary:
;;
;; Validate matlab-ts-mode font-lock faces.
;; Load ../matlab-ts-mode.el via require and run font-lock tests using
;; ./test-matlab-ts-mode-font-lock-files/NAME.m comparing against
;; ./test-matlab-ts-mode-font-lock-files/NAME_expected.txt
;;

;;; Code:

(require 't-utils)
(require 'matlab-ts-mode)

(cl-defun test-matlab-ts-mode-font-lock (&optional m-file)
  "Test font-lock using ./test-matlab-ts-mode-font-lock-files/NAME.m.
Compare ./test-matlab-ts-mode-font-lock-files/NAME.m against
./test-matlab-ts-mode-font-lock-files/NAME_expected.txt, where
NAME_expected.txt is of same length as NAME.m where each source
character in NAME.m is replaced with a character code representing the
font-lock face used for said source character.  The mapping is defined
by the code-to-face alist setup by this function.  If M-FILE is not
provided, loop comparing all
./test-matlab-ts-mode-font-lock-files/NAME.m files.

To add a test, create
  ./test-matlab-ts-mode-font-lock-files/NAME.m
and run this function.  The baseline is saved for you as
  ./test-matlab-ts-mode-font-lock-files/NAME_expected.m~
after validating it, rename it to
  ./test-matlab-ts-mode-font-lock-files/NAME_expected.m"

  (let ((test-name "test-matlab-ts-mode-font-lock")
        (matlab-ts-mode-font-lock-level 4))
    (when (not (t-utils-is-treesit-available 'matlab test-name))
      (cl-return-from test-matlab-ts-mode-font-lock))

    (let* ((m-files (t-utils-get-files (concat test-name "-files") "\\.m$" nil m-file))
           (code-to-face '(
                           ("!" . matlab-ts-mode-system-command-face)
                           ("a" . matlab-ts-mode-command-arg-face)
                           ("b" . font-lock-bracket-face)
                           ("B" . font-lock-builtin-face)
                           ("c" . font-lock-comment-face)
                           ("C" . font-lock-comment-delimiter-face)
                           ("d" . default)
                           ("D" . font-lock-delimiter-face)
                           ("E" . font-lock-escape-face)
                           ("f" . font-lock-function-name-face)
                           ("F" . font-lock-function-call-face)
                           ("h" . font-lock-doc-face) ;; function doc help comment
                           ("H" . matlab-ts-mode-comment-heading-face)
                           ("k" . font-lock-keyword-face)
                           ("M" . matlab-ts-mode-comment-to-do-marker-face)
                           ("n" . matlab-ts-mode-number-face)
                           ("s" . font-lock-string-face)
                           ("S" . matlab-ts-mode-string-delimiter-face)
                           ("o" . matlab-ts-mode-operator-face)
                           ("p" . matlab-ts-mode-pragma-face)
                           ("P" . matlab-ts-mode-property-face)
                           ("t" . font-lock-type-face)
                           ("v" . font-lock-variable-name-face)
                           ("w" . font-lock-warning-face)
                           )))
      (t-utils-test-font-lock test-name m-files code-to-face))
    "success"))

(provide 'test-matlab-ts-mode-font-lock)
;;; test-matlab-ts-mode-font-lock.el ends here
