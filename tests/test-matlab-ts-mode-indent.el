;;; test-matlab-ts-mode-indent.el --- -*- lexical-binding: t -*-
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
;;

;;; Code:

(require 't-utils)
(require 'matlab-ts-mode)

(defvar test-matlab-ts-mode-indent--current-indent-level nil)

(defvar test-matlab-ts-mode-indent--file nil)

(defun test-matlab-ts-mode-indent--file (m-file)
  "Test an individual M-FILE.
This is provided for debugging.
  M-: (test-matlab-ts-mode-indent--file \"test-matlab-ts-mode-indent-files/M-FILE\")"
  (let ((test-matlab-ts-mode-indent--file m-file))
    (ert-run-tests-interactively "test-matlab-ts-mode-indent")))

(ert-deftest test-matlab-ts-mode-indent ()
  "Test indent.
Compare indent of ./test-MATLAB-ts-mode-indent-files/NAME.m
against ./test-MATLAB-ts-mode-indent-files/NAME_expected.m.
Indent is done several ways as described in `t-utils-test-indent'.
This loops on all ./test-MATLAB-ts-mode-indent-files/NAME.m
files.

To add a test, create
  ./test-MATLAB-ts-mode-indent-files/NAME.m
and run this function.  The baseline is saved for you as
  ./test-MATLAB-ts-mode-indent-files/NAME_expected.m~
  ./test-MATLAB-ts-mode-indent-files/NAME_expected_msgs.m~
after validating them, rename them to
  ./test-MATLAB-ts-mode-indent-files/NAME_expected.m
  ./test-MATLAB-ts-mode-indent-files/NAME_expected_msgs.m"

  (let* ((matlab-ts-mode-electric-ends nil)
         (test-name "test-matlab-ts-mode-indent")
         (m-files (t-utils-get-files
                   test-name
                   (rx ".m" eos)
                   nil
                   test-matlab-ts-mode-indent--file))
         (indent-checker (lambda ()
                           (setq test-matlab-ts-mode-indent--current-indent-level
                                 matlab-ts-mode--function-indent-level)))
         (line-manipulator (lambda ()
                             ;; Set the indent level to what the m-file has
                             (setq matlab-ts-mode--function-indent-level
                                   test-matlab-ts-mode-indent--current-indent-level)
                             ;; Workaround
                             ;; https://github.com/acristoffers/tree-sitter-matlab/issues/32
                             (goto-char (point-min))
                             (while (not (eobp))
                               (let* ((node   (treesit-node-at (point)))
                                      (parent (and node (treesit-node-parent node))))
                                 (when (string= (treesit-node-type parent) "ERROR")
                                   (insert " ")))
                               (forward-line)))))
    (t-utils-error-if-no-treesit-for 'matlab test-name)
    (t-utils-test-indent test-name m-files
                         :indent-checker indent-checker
                         :line-manipulator line-manipulator)))

(provide 'test-matlab-ts-mode-indent)
;;; test-matlab-ts-mode-indent.el ends here

;; LocalWords:  utils defun eos setq eobp treesit
