;;; test-matlab-ts-mode-imenu.el --- -*- lexical-binding: t -*-
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
;; ./test-matlab-ts-mode-imenu-files/NAME.m comparing against
;; ./test-matlab-ts-mode-imenu-files/NAME_expected.txt
;;

;;; Code:

(require 't-utils)
(require 'matlab-ts-mode)

(defvar test-matlab-ts-mode-imenu--file nil)

(defun test-matlab-ts-mode-imenu--file (m-file)
  "Test an individual M-FILE.
This is provided for debugging.
  M-: (test-matlab-ts-mode-imenu--file \"test-matlab-ts-mode-imenu-files/M-FILE\")"
  (let ((test-matlab-ts-mode-imenu--file m-file))
    (ert-run-tests-interactively "test-matlab-ts-mode-imenu")))

(ert-deftest test-matlab-ts-mode-imenu ()
  "Test imenu using ./test-matlab-ts-mode-imenu-files/NAME.m.
Using ./test-matlab-ts-mode-imenu-files/NAME.m, compare imenu results
against ./test-matlab-ts-mode-imenu-files/NAME_expected.txt.  This loops
on all ./test-matlab-ts-mode-imenu-files/NAME.m files.

To add a test, create
  ./test-matlab-ts-mode-imenu-files/NAME.m
and run this function.  The baseline is saved for you as
  ./test-matlab-ts-mode-imenu-files/NAME_expected.txt~
after validating it, rename it to
  ./test-matlab-ts-mode-imenu-files/NAME_expected.txt"

  (let ((test-name "test-matlab-ts-mode-imenu"))
    (when (t-utils-is-treesit-available 'matlab test-name)
      (let ((m-files (t-utils-get-files (concat test-name "-files") "\\.m\\'" nil
                                        test-matlab-ts-mode-imenu--file)))
        (t-utils-test-imenu test-name m-files)))))

(provide 'test-matlab-ts-mode-imenu)
;;; test-matlab-ts-mode-imenu.el ends here
