;;; metest-imenu.el --- Testing suite for MATLAB Emacs -*- lexical-binding: t -*-
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
;; Tests to exercise the matlab-imenu-generic-expression regexp
;;

;;; Code:

(require 'matlab)

(defun metest-imenu-files ()
  "Return list of full paths to each metest-imenu-files/*.m."
  (directory-files "metest-imenu-files" t "\\.m$"))

(defvar metest-imenu (cons "metest-imenu" (metest-imenu-files)))

(defun metest-imenu (&optional m-file)
  "Test MATLAB imenu support using ./metest-imenu-files/M-FILE.
Compare ./metest-imenu-files/M-FILE against
./metest-imenu-files/NAME_expected.txt, where NAME_expected.txt
contains the matched functions of the matlab-imenu-generic-expression
regexp, one per line.

If M-FILE is not provided, loop comparing all
  ./metest-imenu-files/*.m

For debugging, you can run with a specified M-FILE,
  M-: (metest-imenu \"metest-imenu-files/M-FILE\")"
  (let* ((m-files (if m-file
                      (progn
                        (setq m-file (file-truename m-file))
                        (when (not (file-exists-p m-file))
                          (error "File %s does not exist" m-file))
                        (list m-file))
                    (metest-imenu-files))))
    (dolist (m-file m-files)
      (save-excursion
        (message "START: (metest-imenu \"%s\")" m-file)

        (let ((m-file-buf (find-file m-file)))
          (with-current-buffer m-file-buf

            (let* ((index (matlab--imenu-index))
                   (got (concat (string-join
                                 (mapcar (lambda (el) (substring-no-properties (car el))) index)
                                 "\n")
                                "\n"))
                   (expected-file (replace-regexp-in-string "\\.m$" "_expected.txt" m-file))
                   (got-file (concat expected-file "~"))
                   (expected (when (file-exists-p expected-file)
                               (with-temp-buffer
                                 (insert-file-contents-literally expected-file)
                                 (buffer-string)))))

              (when (not (string= got expected))
                (let ((coding-system-for-write 'raw-text-unix))
                  (write-region got nil got-file))
                (when (not expected)
                  (error "Baseline for %s does not exist.
See %s and if it looks good rename it to %s"
                         m-file got-file expected-file))
                (error "Baseline for %s does not match, got: %s, expected: %s"
                       m-file got-file expected-file))))
          (kill-buffer m-file-buf))
        (message "PASS: (metest-imenu \"%s\")" m-file))))
  "success")

(provide 'metest-imenu)
;;; metest-imenu.el ends here
