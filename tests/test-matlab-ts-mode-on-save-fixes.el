;;; test-matlab-ts-mode-on-save-fixes.el --- -*- lexical-binding: t -*-

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
;; Tests for matlab-ts-mode on save fixes (fix function/classdef name to match file)
;;

;;; Code:

(require 't-utils)
(require 'matlab-ts-mode)

(defvar test-matlab-ts-mode-on-save-fixes--file nil)

(defun test-matlab-ts-mode-on-save-fixes--file (m-file)
  "Test an individual M-FILE.
This is provided for debugging.
  M-: (test-matlab-ts-mode-on-save-fixes--file
      \"test-matlab-ts-mode-on-save-fixes-files/M-FILE\")"
  (let ((test-matlab-ts-mode-on-save-fixes--file m-file))
    (ert-run-tests-interactively "^test-matlab-ts-mode-on-save-fixes$")))

(ert-deftest test-matlab-ts-mode-on-save-fixes ()
  "Test fix of fcn name using ./test-matlab-ts-mode-on-save-fixes-files/NAME.m.
Using ./test-matlab-ts-mode-on-save-fixes-files/NAME.m, set the
buffer name to \"tmp__NAME.m\" and validate
`matlab-ts-mode-on-save-fix-name' returns expected result found in
./test-matlab-ts-mode-on-save-fixes-files/NAME_expected.org.  This loops
on all ./test-matlab-ts-mode-on-save-fixes-files/NAME.m files.

To add a test, create
  ./test-matlab-ts-mode-on-save-fixes-files/NAME.m
and run this function.  The baseline is saved for you as
  ./test-matlab-ts-mode-on-save-fixes-files/NAME_expected.org~
after validating it, rename it to
  ./test-matlab-ts-mode-on-save-fixes-files/NAME_expected.org"

  (let* ((test-name "test-matlab-ts-mode-on-save-fixes")
         (m-files (t-utils-get-files test-name
                                     :base-regexp (rx ".m" eos)
                                     :file-to-use test-matlab-ts-mode-on-save-fixes--file)))
    (t-utils-error-if-no-treesit-for 'matlab test-name)
    (t-utils-test-xr test-name m-files)))

(provide 'test-matlab-ts-mode-on-save-fixes)
;;; test-matlab-ts-mode-on-save-fixes.el ends here

;; LocalWords:  utils defun tmp eos treesit xr
