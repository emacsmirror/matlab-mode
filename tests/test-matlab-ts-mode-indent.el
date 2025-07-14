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
;; Load ../matlab-ts-mode.el via require and run indent tests using
;; ./test-matlab-ts-mode-indent-files/NAME.m comparing against
;; ./test-matlab-ts-mode-indent-files/NAME_expected.m
;;

;;; Code:

(require 't-utils)
(require 'matlab-ts-mode)

(cl-defun test-matlab-ts-mode-indent (&optional m-file)
  "Test indent using ./test-matlab-ts-mode-indent-files/NAME.m.
Compare indent of ./test-matlab-ts-mode-indent-files/NAME.m against
./test-matlab-ts-mode-indent-files/NAME_expected.m.  Indent is done two
ways as described in `t-utils-test-indent'.  If M-FILE is not provided,
loop comparing all ./test-matlab-ts-mode-indent-files/NAME.m files.

To add a test, create
  ./test-matlab-ts-mode-indent-files/NAME.m
and run this function.  The baseline is saved for you as
  ./test-matlab-ts-mode-indent-files/NAME_expected.m~
after validating it, rename it to
  ./test-matlab-ts-mode-indent-files/NAME_expected.m"

  (let ((test-name "test-matlab-ts-mode-indent")
        (matlab-ts-mode--indent-assert t))

    (when (not (t-utils-is-treesit-available 'matlab test-name))
      (cl-return-from test-matlab-ts-mode-indent))

    (let ((m-files (t-utils-get-files (concat test-name "-files") "\\.m$"
                                      "_expected\\.m$" ;; skip our *_expected.m baselines
                                      m-file))
          (line-manipulator (lambda ()
                              ;; Workaround
                              ;; https://github.com/acristoffers/tree-sitter-matlab/issues/32
                              (goto-char (point-min))
                              (while (not (eobp))
                                (let* ((node   (treesit-node-at (point)))
                                       (parent (and node (treesit-node-parent node))))
                                  (when (string= (treesit-node-type parent) "ERROR")
                                    (insert " ")))
                                (forward-line)))))

      (t-utils-test-indent test-name m-files line-manipulator)))
    "success")

(provide 'test-matlab-ts-mode-indent)
;;; test-matlab-ts-mode-indent.el ends here
