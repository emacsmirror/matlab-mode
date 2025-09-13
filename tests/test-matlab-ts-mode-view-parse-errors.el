;;; test-matlab-ts-mode-view-parse-errors.el --- -*- lexical-binding: t -*-

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
;; Test matlab-ts-mode-view-parse-errors
;;

;;; Code:

(require 't-utils)
(require 'matlab-ts-mode)

(defvar test-matlab-ts-mode-view-parse-errors--file nil)

(defun test-matlab-ts-mode-view-parse-errors--file (m-file)
  "Test an individual M-FILE.
This is provided for debugging.
  M-: (test-matlab-ts-mode-view-parse-errors--file
      \"test-matlab-ts-mode-view-parse-errors-files/M-FILE\")"
  (let ((test-matlab-ts-mode-view-parse-errors--file m-file))
    (ert-run-tests-interactively "test-matlab-ts-mode-view-parse-errors")))

(defun test-matlab-ts-mode-view-parse-errors-action-fun ()
  "Exercise `matlab-ts-mode-view-parse-errors' on the current buffer.
Returns string result which is the contents of the
  \"*parse errors for NAME*\"
buffer created by `matlab-ts-mode-view-parse-errors'."
  (let* ((m-file (file-name-nondirectory t-utils--buf-file))
         (parse-errors-buf (matlab-ts-view-parse-errors))
         (buf-name (buffer-name))
         (contents (with-current-buffer parse-errors-buf
                     (buffer-substring-no-properties (point-min) (point-max))))
         (result (replace-regexp-in-string (rx bol (literal buf-name) ":")
                                           (concat m-file ":")
                                           contents)))
    (kill-buffer parse-errors-buf)
    result))

(ert-deftest test-matlab-ts-mode-view-parse-errors ()
  "Test `matlab-ts-mode-view-parse-errors'.
Using ./test-matlab-ts-mode-view-parse-errors-files/NAME.m, compare
`matlab-ts-mode-view-parse-errors' against
./test-matlab-ts-mode-view-parse-errors-files/NAME_expected.txt.  This loops
on all ./test-matlab-ts-mode-view-parse-errors-files/NAME.m files.

To add a test, create
  ./test-matlab-ts-mode-view-parse-errors-files/NAME.m
and run this function.  The baseline is saved for you as
  ./test-matlab-ts-mode-view-parse-errors-files/NAME_expected.txt~
after validating it, rename it to
  ./test-matlab-ts-mode-view-parse-errors-files/NAME_expected.txt"

  (let* ((test-name "test-matlab-ts-mode-view-parse-errors")
         (m-files (t-utils-get-files
                   test-name
                   (rx ".m" eos)
                   nil
                   test-matlab-ts-mode-view-parse-errors--file)))
    (t-utils-error-if-no-treesit-for 'matlab test-name)
    (t-utils-test-action test-name m-files
                         :action-fun #'test-matlab-ts-mode-view-parse-errors-action-fun)))

(provide 'test-matlab-ts-mode-view-parse-errors)
;;; test-matlab-ts-mode-view-parse-errors.el ends here

;; LocalWords:  utils defun nondirectory buf bol treesit eos
