;;; test-matlab-ts-mode-movement.el --- -*- lexical-binding: t -*-

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
;; Tests for matlab-ts-mode "movement" commands such as C-M-f
;;

;;; Code:

(require 't-utils)
(require 'matlab-ts-mode)

(defvar test-matlab-ts-mode-movement--file nil)

(defun test-matlab-ts-mode-movement--file (m-file)
  "Test an individual M-FILE.
This is provided for debugging.
  M-: (test-matlab-ts-mode-movement--file
      \"test-matlab-ts-mode-movement-files/M-FILE\")"
  (let ((test-matlab-ts-mode-movement--file m-file))
    (ert-run-tests-interactively "^test-matlab-ts-mode-movement$")))

(ert-deftest test-matlab-ts-mode-movement ()
  "Test movement commands, C-M-f, etc.
Using ./test-matlab-ts-mode-movement-files/NAME.m, compare
movement of `matlab-mode-ts-beginning-of-command' and
`matlab-mode-ts-end-of-command' against baseline:
./test-matlab-ts-mode-movement-files/NAME_expected.org.
This loops on all
./test-matlab-ts-mode-movement-files/NAME.m files.

To add a test, create
  ./test-matlab-ts-mode-movement-files/NAME.m
and run this function.  The baseline is saved for you as
  ./test-matlab-ts-mode-movement-files/NAME_expected.org~
after validating it, rename it to
  ./test-matlab-ts-mode-movement-files/NAME_expected.org"

  (let* ((test-name "test-matlab-ts-mode-movement")
         (m-files (t-utils-get-files test-name
                                     :base-regexp (rx ".m" eos)
                                     :file-to-use test-matlab-ts-mode-movement--file)))
    (t-utils-error-if-no-treesit-for 'matlab test-name)
    (t-utils-test-xr test-name m-files)))

(provide 'test-matlab-ts-mode-movement)
;;; test-matlab-ts-mode-movement.el ends here

;; LocalWords:  utils defun eos treesit xr
