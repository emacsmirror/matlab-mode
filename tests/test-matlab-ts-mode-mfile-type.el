;;; test-matlab-ts-mode-mfile-type.el --- -*- lexical-binding: t -*-
;;
;; Copyright (C) 2025 Free Software Foundation, Inc.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;

;;; Commentary:
;;
;; Tests for matlab-ts-mode mfile type handling (script, function, class, empty)
;;

;;; Code:

(require 't-utils)
(require 'matlab-ts-mode)

(defvar test-matlab-ts-mode-mfile-type--file nil)

(defun test-matlab-ts-mode-mfile-type--file (m-file)
  "Test an individual M-FILE.
This is provided for debugging.
  M-: (test-matlab-ts-mode-mfile-type--file
      \"test-matlab-ts-mode-mfile-type-files/M-FILE\")"
  (let ((test-matlab-ts-mode-mfile-type--file m-file))
    (ert-run-tests-interactively "test-matlab-ts-mode-mfile-type")))

(ert-deftest test-matlab-ts-mode-mfile-type ()
  "Test `matlab-ts-mode--mfile-type'.
Using ./test-matlab-ts-mode-mfile-type-files/NAME.m, which contain
`t-utils-xr' commands in comments that run `matlab-ts-mode--mfile-type'
to get the mfile type.  This is compared against baselines:
./test-matlab-ts-mode-mfile-type-files/NAME_expected.org.  This loops on
all ./test-matlab-ts-mode-mfile-type-files/NAME.m files.

To add a test, create
  ./test-matlab-ts-mode-mfile-type-files/NAME.m
and run this function.  The baseline is saved for you as
  ./test-matlab-ts-mode-mfile-type-files/NAME_expected.org~
after validating it, rename it to
  ./test-matlab-ts-mode-mfile-type-files/NAME_expected.org"

  (let* ((test-name "test-matlab-ts-mode-mfile-type")
         (m-files (t-utils-get-files
                   test-name
                   (rx ".m" eos)
                   nil
                   test-matlab-ts-mode-mfile-type--file)))
    (t-utils-error-if-no-treesit-for 'matlab test-name)
    (t-utils-test-xr test-name m-files)))

(provide 'test-matlab-ts-mode-mfile-type)
;;; test-matlab-ts-mode-mfile-type.el ends here

;; LocalWords:  utils defun xr eos treesit
