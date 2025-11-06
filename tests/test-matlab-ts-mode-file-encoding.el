;;; test-matlab-ts-mode-file-encoding.el --- -*- lexical-binding: t -*-

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
;; Tests for matlab-ts-mode for file encoding
;;

;;; Code:

(require 't-utils)
(require 'matlab-ts-mode)

(defvar test-matlab-ts-mode-file-encoding--file nil)

(defun test-matlab-ts-mode-file-encoding--file (m-file)
  "Test an individual M-FILE.
This is provided for debugging.
  M-: (test-matlab-ts-mode-file-encoding--file \"test-matlab-ts-mode-file-encoding-files/M-FILE\")"
  (let ((test-matlab-ts-mode-file-encoding--file m-file))
    (ert-run-tests-interactively "test-matlab-ts-mode-file-encoding")))

(ert-deftest test-matlab-ts-mode-file-encoding ()
  "Test file encoding.
Using ./test-matlab-ts-mode-file-encoding-files/NAME.m, compare result
of activating matlab-ts-mode on NAME.m against
./test-matlab-ts-mode-file-encoding-files/NAME_expected.txt.  Where the
expected result is a successful activation or an error message.  This
loops on all ./test-matlab-ts-mode-file-encoding-files/NAME.m files.

To add a test, create
  ./test-matlab-ts-mode-file-encoding-files/NAME.m
and run this function.  The baseline is saved for you as
  ./test-matlab-ts-mode-file-encoding-files/NAME_expected.txt~
after validating it, rename it to
  ./test-matlab-ts-mode-file-encoding-files/NAME_expected.txt"

  (let* ((test-name "test-matlab-ts-mode-file-encoding")
         (m-files (t-utils-get-files test-name
                                     :base-regexp (rx ".m" eos)
                                     :file-to-use test-matlab-ts-mode-file-encoding--file)))
    (t-utils-error-if-no-treesit-for 'matlab test-name)
    (t-utils-test-file-encoding test-name m-files
                                :file-major-mode #'matlab-ts-mode)))

(provide 'test-matlab-ts-mode-file-encoding)
;;; test-matlab-ts-mode-file-encoding.el ends here

;; LocalWords:  utils defun eos treesit
