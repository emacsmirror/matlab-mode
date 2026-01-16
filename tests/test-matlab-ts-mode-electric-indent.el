;;; test-matlab-ts-mode-electric-indent.el --- -*- lexical-binding: t -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.
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
;; Validate matlab-ts-mode indent.
;;

;;; Code:

(require 't-utils)
(require 'matlab-ts-mode)

(defvar test-matlab-ts-mode-electric-indent--current-indent-level nil)

(defvar test-matlab-ts-mode-electric-indent--file nil)

(defun test-matlab-ts-mode-electric-indent--file (m-file)
  "Test an individual M-FILE.
This is provided for debugging.
  M-: (test-matlab-ts-mode-electric-indent--file \
\"test-matlab-ts-mode-electric-indent-files/M-FILE\")"
  (let ((test-matlab-ts-mode-electric-indent--file m-file))
    (ert-run-tests-interactively "^test-matlab-ts-mode-electric-indent$")))

(ert-deftest test-matlab-ts-mode-electric-indent ()
  "Test electric indent.
Compare indent of ./test-MATLAB-ts-mode-electric-indent-files/NAME.m
against ./test-MATLAB-ts-mode-electric-indent-files/NAME_expected.m.
Indent is done several ways as described in `t-utils-test-indent'.
This loops on all ./test-MATLAB-ts-mode-electric-indent-files/NAME.m
files.

To add a test, create
  ./test-MATLAB-ts-mode-electric-indent-files/NAME.m
and run this function.  The baseline is saved for you as
  ./test-MATLAB-ts-mode-electric-indent-files/NAME_expected.m~
  ./test-MATLAB-ts-mode-electric-indent-files/NAME_expected_msgs.m~
after validating them, rename them to
  ./test-MATLAB-ts-mode-electric-indent-files/NAME_expected.m
  ./test-MATLAB-ts-mode-electric-indent-files/NAME_expected_msgs.m"

  (let* ((matlab-ts-mode-electric-ends nil)
         (test-name "test-matlab-ts-mode-electric-indent")
         (m-files (t-utils-get-files test-name
                                     :base-regexp (rx ".m" eos)
                                     :file-to-use test-matlab-ts-mode-electric-indent--file))
         (indent-checker (lambda ()
                           (setq test-matlab-ts-mode-electric-indent--current-indent-level
                                 matlab-ts-mode--function-indent-level)))
         (line-manipulator (lambda ()
                             ;; Set the indent level to what the m-file has
                             (setq matlab-ts-mode--function-indent-level
                                   test-matlab-ts-mode-electric-indent--current-indent-level)))
         ;; Enable electric indent
         (matlab-ts-mode--electric-indent t)
         (matlab-ts-mode--indent-assert t)
         (matlab-ts-mode--electric-indent-verbose nil))
    (t-utils-error-if-no-treesit-for 'matlab test-name)
    (t-utils-test-indent test-name m-files
                         :indent-checker indent-checker
                         :line-manipulator line-manipulator)))

(provide 'test-matlab-ts-mode-electric-indent)
;;; test-matlab-ts-mode-electric-indent.el ends here

;; LocalWords:  utils defun eos setq eobp treesit
