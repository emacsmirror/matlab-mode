;;; metest-imenu-tlc.el --- Testing suite for MATLAB Emacs -*- lexical-binding: t -*-
;;

;; Copyright 2025 Free Software Foundation, Inc.
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
;; Tests to exercise the tlc--imenu-generic-expression regexp
;;

;;; Code:

(require 'tlc)

(defun metest-imenu-tlc-files ()
  "Return list of full paths to each metest-imenu-tlc-files/*.tlc."
  (directory-files "metest-imenu-tlc-files" t "\\.tlc$"))

(defvar metest-imenu-tlc (cons "metest-imenu-tlc" (metest-imenu-tlc-files)))

(defun metest-imenu-tlc (&optional tlc-file)
  "Test MATLAB imenu support using ./metest-imenu-tlc-files/TLC-FILE.
Compare ./metest-imenu-tlc-files/TLC-FILE against
./metest-imenu-tlc-files/NAME_expected.txt, where NAME_expected.txt
contains the matched functions of the tlc--imenu-generic-expression
regexp, one per line.

If TLC-FILE is not provided, loop comparing all
  ./metest-imenu-tlc-files/*.tlc

For debugging, you can run with a specified TLC-FILE,
  M-: (metest-imenu-tlc \"metest-imenu-tlc-files/TLC-FILE\")"
  (let* ((tlc-files (if tlc-file
                      (progn
                        (setq tlc-file (file-truename tlc-file))
                        (when (not (file-exists-p tlc-file))
                          (error "File %s does not exist" tlc-file))
                        (list tlc-file))
                    (metest-imenu-tlc-files))))
    (dolist (tlc-file tlc-files)
      (save-excursion
        (message "START: (metest-imenu-tlc \"%s\")" tlc-file)

        (find-file tlc-file)
        (goto-char (point-min))

        (let* ((imenu-re (cadar tlc--imenu-generic-expression))
               (got "")
               (expected-file (replace-regexp-in-string "\\.tlc$" "_expected.txt" tlc-file))
               (got-file (concat expected-file "~"))
               (expected (when (file-exists-p expected-file)
                           (with-temp-buffer
                             (insert-file-contents-literally expected-file)
                             (buffer-string))))
               (case-fold-search nil))
          (while (re-search-forward imenu-re nil t)
            (setq got (concat got (match-string 1) "\n")))

          (when (not (string= got expected))
            (let ((coding-system-for-write 'raw-text-unix))
              (write-region got nil got-file))
            (when (not expected)
              (error "Baseline for %s does not exists.  See %s and if it looks good rename it to %s"
                     tlc-file got-file expected-file))
            (error "Baseline for %s does not match, got: %s, expected: %s"
                   tlc-file got-file expected-file))
          (kill-buffer)))
      (message "PASS: (metest-imenu-tlc \"%s\")" tlc-file)))
  "success")

(provide 'metest-imenu-tlc)
;;; metest-imenu-tlc.el ends here
