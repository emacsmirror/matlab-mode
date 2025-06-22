;;; test-matlab-ts-mode-syntax-table.el --- -*- lexical-binding: t -*-
;;
;; Copyright 2025 Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses/.

;;; Commentary:
;;
;; Validate matlab-ts-mode syntax faces.
;; Load ../matlab-ts-mode.el via require and run syntax table tests using
;; ./test-matlab-ts-mode-syntax-table-files/NAME.m comparing against
;; ./test-matlab-ts-mode-syntax-table-files/NAME_expected.txt
;;

;;; Code:

(require 'cl-macs)

;; Add abs-path of ".." to load-path so we can (require 'matlab-ts-mode)
(let* ((lf (or load-file-name (buffer-file-name (current-buffer))))
       (d1 (file-name-directory lf))
       (parent-dir (expand-file-name (file-name-directory (directory-file-name d1)))))
  (add-to-list 'load-path parent-dir t))

(require 'matlab-ts-mode)

(defun test-matlab-ts-mode-syntax-table-files ()
  "Return list of full paths to each test-matlab-ts-mode-syntax-table-files/*.m."
  (directory-files "test-matlab-ts-mode-syntax-table-files" t "\\.m$"))

(defvar test-matlab-ts-mode-syntax-table
  (cons "test-matlab-ts-mode-syntax-table" (test-matlab-ts-mode-syntax-table-files)))

(cl-defun test-matlab-ts-mode-syntax-table (&optional m-file)
  "Test syntax-table using ./test-matlab-ts-mode-syntax-table-files/NAME.m.
Compare ./test-matlab-ts-mode-syntax-table-files/NAME.m against
./test-matlab-ts-mode-syntax-table-files/NAME_expected.txt, where
NAME_expected.txt gives the `syntax-ppss` value of each character in NAME.m

If M-FILE NAME.m is not provided, loop comparing all
./test-matlab-ts-mode-syntax-table-files/NAME.m files.

For debugging, you can run with a specified NAME.m,
  M-: (test-matlab-ts-mode-syntax-table \"test-matlab-ts-mode-syntax-table-files/NAME.m\")"

  (when (or (< emacs-major-version 30)
            (not (progn
                   (require 'treesit)
                   (when (fboundp 'treesit-ready-p)
                     (treesit-ready-p 'matlab t)))))
    (message "skipping-test: test-matlab-ts-mode-syntax-table.el - tree sitter not available.")
    (cl-return-from test-matlab-ts-mode-syntax-table))

  (let ((m-files (if m-file
                     (progn
                       (setq m-file (file-truename m-file))
                       (when (not (file-exists-p m-file))
                         (error "File %s does not exist" m-file))
                       (list m-file))
                   (test-matlab-ts-mode-syntax-table-files))))
    (dolist (m-file m-files)
      (save-excursion
        (message "START: test-matlab-ts-mode-syntax-table %s" m-file)

        (find-file m-file)
        (goto-char (point-min))

        (let* ((got "")
               (expected-file (replace-regexp-in-string "\\.m$" "_expected.txt" m-file))
               (got-file (concat expected-file "~"))
               (expected (when (file-exists-p expected-file)
                           (with-temp-buffer
                             (insert-file-contents-literally expected-file)
                             (buffer-string)))))
          (while (not (eobp))
            (when (looking-at "^")
              (setq got (concat got (format "Line:%d: %s\n"
                                            (line-number-at-pos)
                                            (buffer-substring-no-properties (point)
                                                                            (line-end-position))))))
            
            (let ((char (buffer-substring-no-properties (point) (1+ (point)))))
              (when (string= char "\n")
                (setq char "\\n"))
              (setq got (concat got (format "  %2s: %S\n" char (syntax-ppss (point))))))

            (forward-char))

          (when (not (string= got expected))
            (let ((coding-system-for-write 'raw-text-unix))
              (write-region got nil got-file))
            (when (not expected)
              (error "Baseline for %s does not exists.  \
See %s and if it looks good rename it to %s"
                     m-file got-file expected-file))
            (error "Baseline for %s does not match, got: %s, expected: %s"
                   m-file got-file expected-file))
          (kill-buffer)))
      (message "PASS: test-matlab-ts-mode-syntax-table %s" m-file)))
  "success")

(provide 'test-matlab-ts-mode-syntax-table)
;;; test-matlab-ts-mode-syntax-table.el ends here
