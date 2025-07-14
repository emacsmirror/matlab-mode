;;; test-matlab-ts-mode-on-save-fixes.el --- -*- lexical-binding: t -*-
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
;; Validate matlab-ts-mode indent.
;; Load ../matlab-ts-mode.el via require and run indent tests using
;; ./test-matlab-ts-mode-on-save-fixes-files/NAME.m comparing against
;; ./test-matlab-ts-mode-on-save-fixes-files/NAME_expected.org
;;

;;; Code:

(require 't-utils)
(require 'matlab-ts-mode)

(cl-defun test-matlab-ts-mode-on-save-fixes (&optional m-file)
  "Test defun movement using ./test-matlab-ts-mode-on-save-fixes-files/NAME.m.
Using ./test-matlab-ts-mode-on-save-fixes-files/NAME.m, compare defun
movement against
./test-matlab-ts-mode-on-save-fixes-files/NAME_expected.org.  If M-FILE is
not provided, loop comparing all
./test-matlab-ts-mode-on-save-fixes-files/NAME.m files.

To add a test, create
  ./test-matlab-ts-mode-on-save-fixes-files/NAME.m
and run this function.  The baseline is saved for you as
  ./test-matlab-ts-mode-on-save-fixes-files/NAME_expected.org~
after validating it, rename it to
  ./test-matlab-ts-mode-on-save-fixes-files/NAME_expected.org"

  (let ((test-name "test-matlab-ts-mode-on-save-fixes"))

    (when (not (t-utils-is-treesit-available 'matlab test-name))
      (cl-return-from test-matlab-ts-mode-on-save-fixes))

    (let ((m-files (t-utils-get-files (concat test-name "-files") "\\.m$" nil m-file)))
      (t-utils-test-xr test-name m-files)))
    "success")

(provide 'test-matlab-ts-mode-on-save-fixes)
;;; test-matlab-ts-mode-on-save-fixes.el ends here
