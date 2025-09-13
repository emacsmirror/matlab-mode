;;; metest-indent-test2.el --- Testing suite for MATLAB Emacs -*- lexical-binding: t -*-

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
;; Tests to exercise font-lock using ./metest-indent-test2-files/*
;;

;;; Code:

(require 'cl-seq)

(defun metest-indent-test2-files ()
  "Return list of full paths to each metest-indent-test2-files/*.m."
  (cl-delete-if (lambda (m-file)
                  (string-match "_expected\\.m$" m-file))
                (directory-files "metest-indent-test2-files" t "\\.m$")))

(defvar metest-indent-test2 (cons "indent-test2" (metest-indent-test2-files)))

(defun metest-indent-test2 (&optional m-file)
  "Test indent using ./metest-indent-test2-files/M-FILE.
Compare indent of ./metest-indent-test2-files/M-FILE against
./metest-indent-test2-files/NAME_expected.m

If M-FILE is not provided, loop comparing all
  ./metest-indent-test2-files/*.m"
  (let* ((m-files (if m-file
                      (progn
                        (setq m-file (file-truename m-file))
                        (when (not (file-exists-p m-file))
                          (error "File %s does not exist" m-file))
                        (list m-file))
                    (metest-indent-test2-files))))
    (dolist (m-file m-files)
      (save-excursion
        (message "START: metest-indent-test2 %s" m-file)
        (find-file m-file)
        (indent-region (point-min) (point-max))
        (let* ((got (buffer-substring (point-min) (point-max)))
              (expected-file (replace-regexp-in-string "\\.m$" "_expected.m" m-file))
              (got-file (concat expected-file "~"))
              (expected (when (file-exists-p expected-file)
                          (with-temp-buffer
                            (insert-file-contents-literally expected-file)
                            (buffer-string)))))
          (set-buffer-modified-p nil)
          (kill-buffer)
          (when (not (string= got expected))
            (let ((coding-system-for-write 'raw-text-unix))
              (write-region got nil got-file))
            (when (not expected)
              (error "Baseline for %s does not exists.  See %s and if it looks good rename it to %s"
                     m-file got-file expected-file))
            (error "Baseline for %s does not match, got: %s, expected: %s"
                   m-file got-file expected-file))))
      (message "PASS: metest-indent-test2 %s" m-file)))
  "success")
  

(provide 'metest-indent-test2)
;;; metest-indent-test2.el ends here
