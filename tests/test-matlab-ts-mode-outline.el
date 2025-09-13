;;; test-matlab-ts-mode-outline.el --- -*- lexical-binding: t -*-

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
;; Tests for matlab-ts-mode `outline-minor-mode'
;;

;;; Code:

(require 't-utils)
(require 'matlab-ts-mode)

(defvar test-matlab-ts-mode-outline--file nil)

(defun test-matlab-ts-mode-outline--file (m-file)
  "Test an individual M-FILE.
This is provided for debugging.
  M-: (test-matlab-ts-mode-outline--file \"test-matlab-ts-mode-outline-files/M-FILE\")"
  (let ((test-matlab-ts-mode-outline--file m-file))
    (ert-run-tests-interactively "test-matlab-ts-mode-outline")))

(ert-deftest test-matlab-ts-mode-outline ()
  "Test outline mode using ./test-matlab-ts-mode-outline-files/NAME.m.
Using ./test-matlab-ts-mode-outline-files/NAME.m, call `outline-search-function'
and compare result against
./test-matlab-ts-mode-outline-files/NAME_expected.txt.  This loops
on all ./test-matlab-ts-mode-outline-files/NAME.m files.

To add a test, create
  ./test-matlab-ts-mode-outline-files/NAME.m
and run this function.  The baseline is saved for you as
  ./test-matlab-ts-mode-outline-files/NAME_expected.txt~
after validating it, rename it to
  ./test-matlab-ts-mode-outline-files/NAME_expected.txt"

  (let* ((test-name "test-matlab-ts-mode-outline")
         (m-files (t-utils-get-files
                   test-name
                   (rx ".m" eos)
                   nil
                   test-matlab-ts-mode-outline--file)))
    (t-utils-error-if-no-treesit-for 'matlab test-name)
    (t-utils-test-outline-search-function test-name m-files)))

(provide 'test-matlab-ts-mode-outline)
;;; test-matlab-ts-mode-outline.el ends here

;; LocalWords:  utils defun eos treesit
