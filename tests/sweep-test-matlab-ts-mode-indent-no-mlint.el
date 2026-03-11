;;; sweep-test-matlab-ts-mode-indent-no-mlint.el --- -*- lexical-binding: t -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.
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
;; M-: (sweep-test-matlab-ts-mode-indent-no-mlint)
;;
;; Checks indent on all files under the current directory:
;;  - runs `t-utils-sweep-test-indent' which indent files, saving as NAME~ and creates a diff.
;;  - activates `matlab-ts-mode--indent-assert'
;;  - reports slowest indents

;;; Code:

(require 't-utils)
(require 'matlab-ts-mode)
(require 'matlab--access)

(defun sweep-test-matlab-ts-mode-indent-no-mlint (&optional directory save-diff)
  "Use `matlab-ts-mode' to indent each *.m file in DIRECTORY.

If DIRECTORY isn't specified, it defaults to the current directory.

This calls `t-utils-sweep-test-indent' with does a number of checks to
validate the indent rules.  When run interactively, displays the result
in a *sweep-test-matlab-ts-mode-indent-no-mlint* buffer, otherwise the
results are displayed on stdout.  When SAVE-DIFF is t, a *.diff file is
created."

  (let ((test-name "sweep-test-matlab-ts-mode-indent-no-mlint")
        (matlab-ts-mode--indent-assert t))

    (add-to-list 'major-mode-remap-alist '(matlab-mode . matlab-ts-mode))

    (t-utils-sweep-test-indent
     test-name
     (or directory default-directory)
     (rx ".m" eos)
     #'matlab-ts-mode
     :save-diff save-diff)))

(provide 'sweep-test-matlab-ts-mode-indent-no-mlint)
;;; sweep-test-matlab-ts-mode-indent-no-mlint.el ends here

;; LocalWords:  utils defun alist eos
