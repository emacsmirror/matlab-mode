;;; sweep-test-matlab-ts-mode-indent.el --- -*- lexical-binding: t -*-
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
;; ./sweep-test-matlab-ts-mode-indent-files/NAME.m comparing against
;; ./sweep-test-matlab-ts-mode-indent-files/NAME_expected.m
;;

;;; Code:

(require 't-utils)
(require 'matlab-ts-mode)
(require 'matlab--access)

(defvar sweep-test-matlab-ts-mode-indent--mlint (or (matlab--get-mlint-exe)
                                                    (error "MLint not found")))

(defun sweep-test-matlab-ts-mode-indent--syntax-checker (file)
  "MLint FILE, return pair (VALID . CHECK-RESULT).
Where VALID is t if the file has valid syntax, nil otherwise.
String CHECK-RESULT is what the MLint returned."
  (let ((mlint-out (shell-command-to-string
                    (concat (shell-quote-argument sweep-test-matlab-ts-mode-indent--mlint)
                            " "
                            (shell-quote-argument file))))
        valid)

    (setq valid
          (not
           (string-match-p
            (rx
             (or
              ;; L 2 (C 15): Parse error at <EOL>: usage might be invalid MATLAB syntax.
              "Parse error"
              ;; L 22 (C 10): A '(' might be missing a closing ')', causing invalid syntax at \
              ;;              end of line.
              "invalid syntax"
              ;; L 12 (C 13-15): Invalid use of a reserved word.
              "Invalid use of reserved word"
              ;; L 4 (C 1-8): An END might be missing (after line 8), possibly matching CLASSDEF.
              "An END might be missing"
              ;; L 195 (C 15): Invalid text character(s).
              ;;    regularcode;  #r#
              "Invalid text character"))
            mlint-out)))
    (cons valid mlint-out)))

(defun sweep-test-matlab-ts-mode-indent (&optional directory check-valid-parse)
  "Use `matlab-ts-mode' to indent each *.m file in DIRECTORY.

If DIRECTORY isn't specified, it defaults to the current directory.

CHECK-VALID-PARSE if t, will call SYNTAX-CHECKER-FUN on all files being
processed to verify that the a successful tree-sitter parse also has no
errors according to SYNTAX-CHECKER-FUN.  Any inconsistent parses are
reported which is likely a bug in the tree-sitter parser.

This calls `t-utils-sweep-test-indent' with does a number of
checks to validate the ident rules.  When run interactively,
displays the result in a *t-utils-seep-indent* buffer, otherwise
the results are displayed on stdout."

  (let ((test-name "sweep-test-matlab-ts-mode-indent")
        (matlab-ts-mode--indent-assert t))

    (t-utils-sweep-test-indent test-name
                               (or directory default-directory)
                               (rx ".m" eos)
                               #'matlab-ts-mode
                               #'sweep-test-matlab-ts-mode-indent--syntax-checker
                               check-valid-parse
                               (rx bos "ERROR" eos))))

(provide 'sweep-test-matlab-ts-mode-indent)
;;; sweep-test-matlab-ts-mode-indent.el ends here
