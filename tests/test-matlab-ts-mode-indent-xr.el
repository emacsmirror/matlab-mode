;;; test-matlab-ts-mode-indent-xr.el --- -*- lexical-binding: t -*-
;;
;; Copyright 2025 Free Software Foundation, Inc.
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
;; Run indent tests using `t-utils-xr' in
;;   ./test-matlab-ts-mode-indent-xr-files/NAME.m
;; comparing against
;;   ./test-matlab-ts-mode-indent-xr-files/NAME_expected.org
;;

;;; Code:

(require 't-utils)
(require 'matlab-ts-mode)

(defvar test-matlab-ts-mode-indent-xr--file nil)

(defun test-matlab-ts-mode-indent-xr--file (m-file)
  "Test an individual M-FILE.
This is provided for debugging.
  M-: (test-matlab-ts-mode-indent-xr--file
      \"test-matlab-ts-mode-indent-xr-files/M-FILE\")"
  (let ((test-matlab-ts-mode-indent-xr--file m-file))
    (ert-run-tests-interactively "test-matlab-ts-mode-indent-xr")))

(ert-deftest test-matlab-ts-mode-indent-xr ()
  "Test indent using ./test-matlab-ts-mode-indent-xr-files/NAME.m.
Using ./test-matlab-ts-mode-indent-xr-files/NAME.m, compare typing
commands via `t-utils-xr' Lisp commans in the *.m files and compare
agains ./test-matlab-ts-mode-indent-xr-files/NAME_expected.org.  This
loops on all ./test-matlab-ts-mode-indent-xr-files/NAME.m files.

To add a test, create
  ./test-matlab-ts-mode-indent-xr-files/NAME.m
and run this function.  The baseline is saved for you as
  ./test-matlab-ts-mode-indent-xr-files/NAME_expected.org~
after validating it, rename it to
  ./test-matlab-ts-mode-indent-xr-files/NAME_expected.org"

  (let* ((matlab-ts-mode-electric-ends nil)
         (test-name "test-matlab-ts-mode-indent-xr")
         (m-files (t-utils-get-files
                   test-name
                   (rx ".m" eos)
                   nil
                   test-matlab-ts-mode-indent-xr--file)))
    (t-utils-error-if-no-treesit-for 'matlab test-name)
    (t-utils-test-xr test-name m-files)))

(provide 'test-matlab-ts-mode-indent-xr)
;;; test-matlab-ts-mode-indent-xr.el ends here
