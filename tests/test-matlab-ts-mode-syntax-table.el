;;; test-matlab-ts-mode-syntax-table.el --- -*- lexical-binding: t -*-
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
;; Validate matlab-ts-mode syntax faces.
;; Load ../matlab-ts-mode.el via require and run syntax table tests using
;; ./test-matlab-ts-mode-syntax-table-files/NAME.m comparing against
;; ./test-matlab-ts-mode-syntax-table-files/NAME_expected.txt
;;

;;; Code:

(require 't-utils)
(require 'matlab-ts-mode)

(cl-defun test-matlab-ts-mode-syntax-table (&optional m-file)
  "Test syntax-table using ./test-matlab-ts-mode-syntax-table-files/NAME.m.
Compare ./test-matlab-ts-mode-syntax-table-files/NAME.m against
./test-matlab-ts-mode-syntax-table-files/NAME_expected.txt, where
NAME_expected.txt gives the `syntax-ppss` value of each character in
NAME.m.  If M-FILE is not provided, loop comparing all
./test-matlab-ts-mode-indent-files/NAME.m files.

To add a test, create
  ./test-matlab-ts-mode-syntax-table-files/NAME.m
and run this function.  The baseline is saved for you as
  ./test-matlab-ts-mode-syntax-table-files/NAME_expected.m~
after validating it, rename it to
  ./test-matlab-ts-mode-syntax-table-files/NAME_expected.m"

  (let ((test-name "test-matlab-ts-mode-syntax-table"))
    (when (not (t-utils-is-treesit-available 'matlab test-name))
      (cl-return-from test-matlab-ts-mode-syntax-table))

    (let ((m-files (t-utils-get-files (concat test-name "-files") "\\.m$" nil m-file)))
      (t-utils-test-syntax-table test-name m-files)))

  "success")

(provide 'test-matlab-ts-mode-syntax-table)
;;; test-matlab-ts-mode-syntax-table.el ends here
