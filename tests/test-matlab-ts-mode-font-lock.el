;;; test-matlab-ts-mode-font-lock.el --- -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.
;;
;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Tests for matlab-ts-mode for font-lock
;;

;;; Code:

(require 't-utils)
(require 'matlab-ts-mode)

(defvar test-matlab-ts-mode-font-lock--file nil)

(defun test-matlab-ts-mode-font-lock--file (m-file)
  "Test an individual M-FILE.
This is provided for debugging.
  M-: (test-matlab-ts-mode-font-lock--file \"test-matlab-ts-mode-font-lock-files/M-FILE\")"
  (let ((test-matlab-ts-mode-font-lock--file m-file))
    (ert-run-tests-interactively "^test-matlab-ts-mode-font-lock$")))

(ert-deftest test-matlab-ts-mode-font-lock ()
  "Test font-lock using ./test-matlab-ts-mode-font-lock-files/NAME.m.
Compare font of ./test-matlab-ts-mode-font-lock-files/NAME.m against
./test-matlab-ts-mode-font-lock-files/NAME_expected.txt, where
NAME_expected.txt is of same length as NAME.m where each source
character in NAME.m is replaced with a character code representing the
font-lock face used for said source character.  The mapping is defined
by the code-to-face alist setup by this function.  This loops
on all ./test-matlab-ts-mode-font-lock-files/NAME.m files.

To add a test, create
  ./test-matlab-ts-mode-font-lock-files/NAME.m
and run this function.  The baseline is saved for you as
  ./test-matlab-ts-mode-font-lock-files/NAME_expected.txt~
after validating it, rename it to
  ./test-matlab-ts-mode-font-lock-files/NAME_expected.txt"

  (let* ((test-name "test-matlab-ts-mode-font-lock")
         (matlab-ts-mode-font-lock-level 4)
         (m-files (t-utils-get-files test-name
                                     :base-regexp (rx ".m" eos)
                                     :file-to-use test-matlab-ts-mode-font-lock--file))
         (code-to-face '(
                         ("!" . matlab-ts-mode-system-command-face)
                         ("a" . matlab-ts-mode-command-arg-face)
                         ("b" . font-lock-bracket-face)
                         ("B" . font-lock-builtin-face)
                         ("C" . font-lock-comment-delimiter-face)
                         ("c" . font-lock-comment-face)
                         ("d" . default)
                         ("D" . font-lock-delimiter-face)
                         ("E" . font-lock-escape-face)
                         ("F" . font-lock-function-call-face)
                         ("f" . font-lock-function-name-face)
                         ("g" . matlab-function-signature-face)
                         ("h" . font-lock-doc-face) ;; function doc help comment
                         ("H" . matlab-ts-mode-comment-heading-face)
                         ("k" . font-lock-keyword-face)
                         ("M" . matlab-ts-mode-comment-to-do-marker-face)
                         ("n" . matlab-ts-mode-number-face)
                         ("N" . matlab-ts-mode-end-number-face)
                         ("o" . matlab-ts-mode-operator-face)
                         ("p" . matlab-ts-mode-pragma-face)
                         ("P" . matlab-ts-mode-property-face)
                         ("s" . font-lock-string-face)
                         ("S" . matlab-ts-mode-string-delimiter-face)
                         ("t" . font-lock-type-face)
                         ("v" . font-lock-variable-name-face)
                         ("V" . matlab-ts-mode-variable-override-builtin-face)
                         ("w" . font-lock-warning-face)
                         ))
         (test-buf-setup (lambda () (matlab-sections-minor-mode -1))))
    (t-utils-error-if-no-treesit-for 'matlab test-name)
    (t-utils-test-font-lock test-name m-files
                            :code-to-face code-to-face
                            :setup-callback test-buf-setup)))

(provide 'test-matlab-ts-mode-font-lock)
;;; test-matlab-ts-mode-font-lock.el ends here

;; LocalWords:  utils defun alist eos buf treesit
