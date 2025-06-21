;;; test-matlab-ts-mode-indent.el --- Test matlab-ts-mode indent -*- lexical-binding: t -*-
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

(require 'cl-seq)

;; Add abs-path of ".." to load-path so we can (require 'matlab-ts-mode)
(let* ((lf (or load-file-name (buffer-file-name (current-buffer))))
       (d1 (file-name-directory lf))
       (parent-dir (expand-file-name (file-name-directory (directory-file-name d1)))))
  (add-to-list 'load-path parent-dir t))

(require 'matlab-ts-mode)

(setq matlab-ts-mode--indent-assert t)

(defun test-matlab-ts-mode-indent-files ()
  "Return list of full paths to each test-matlab-ts-mode-indent-files/*.m."
  (cl-delete-if (lambda (m-file)
                  (string-match "_expected\\.m$" m-file))
                (directory-files "test-matlab-ts-mode-indent-files" t "\\.m$")))

(defvar test-matlab-ts-mode-indent (cons "test-matlab-ts-mode-indent"
                                         (test-matlab-ts-mode-indent-files)))

(defun trim ()
  "Trim trailing whitespace and lines."
  (setq buffer-file-coding-system 'utf-8-unix)
  (let ((delete-trailing-lines t))
    (delete-trailing-whitespace (point-min) (point-max))))

(defun test-matlab-ts-mode-indent--typing (m-file expected expected-file)
  "Exercise indent by simulating the creation of M-FILE via typing.
This compares the simulation of typing M-FILE line by line against
EXPECTED content in EXPECTED-FILE."

  (message "START: test-matlab-ts-mode-indent (typing) %s" m-file)

  (let* ((typing-m-file-name (concat "typing__" (file-name-nondirectory m-file)))
         (contents (with-temp-buffer
                     (insert-file-contents-literally m-file)
                     (buffer-substring (point-min) (point-max))))
         (lines (split-string (string-trim contents) "\n")))
    (with-current-buffer (get-buffer-create typing-m-file-name)
      (erase-buffer)
      (matlab-ts-mode)

      ;; Insert the non-empty lines into typing-m-file-name buffer
      (dolist (line lines)
        (setq line (string-trim line))
        (when (not (string= line ""))
          (insert line "\n")))

      ;; Now indent each line and insert the empty ("") lines into typing-m-file-buffer
      ;; as we indent. This exercises the RET and TAB behaviors which cause different
      ;; tree-sitter nodes to be provided to the indent engine rules.
      (goto-char (point-min))
      (while (not (eobp))

        ;; Workaround https://github.com/acristoffers/tree-sitter-matlab/issues/32
       (let* ((node   (treesit-node-at (point)))
              (parent (and node (treesit-node-parent node))))
         (when (string= (treesit-node-type parent) "ERROR")
           (insert " ")))
               
        (call-interactively #'indent-for-tab-command) ;; TAB on code just added

        ;; While next line in our original contents is a newline insert "\n"
        (while (let ((next-line (nth (line-number-at-pos (point)) lines)))
                 (and next-line (string-match-p "^[ \t\r]*$" next-line)))
          (goto-char (line-end-position))
          ;; RET to add blank line
          (call-interactively #'newline)
          ;; TAB on the same blank line can result in different tree-sitter nodes than
          ;; the RET, so exercise that.
          (call-interactively #'indent-for-tab-command))
        (forward-line))

      (trim)

      (let ((typing-got (buffer-substring (point-min) (point-max))))
        (set-buffer-modified-p nil)
        (kill-buffer)
        (when (not (string= typing-got expected))
          (let ((coding-system-for-write 'raw-text-unix)
                (typing-got-file (replace-regexp-in-string "\\.m$" "_typing.m~" m-file)))
            (write-region typing-got nil typing-got-file)
            (error "Typing %s line-by-line does not match %s, we got %s" m-file expected-file
                   typing-got-file)))))))

(defun test-matlab-ts-mode-indent (&optional m-file)
  "Test indent using ./test-matlab-ts-mode-indent-files/NAME.m.
Compare indent of ./test-matlab-ts-mode-indent-files/NAME.m against
./test-matlab-ts-mode-indent-files/NAME_expected.m

If M-FILE (NAME.m) is not provided, loop comparing all
./test-matlab-ts-mode-indent-files/NAME.m files.

For debugging, you can run with a specified NAME.m,
  M-: (test-matlab-ts-mode-font-lock \"test-matlab-ts-mode-indent-files/NAME.m\")"
  
  (let* ((m-files (if m-file
                      (progn
                        (setq m-file (file-truename m-file))
                        (when (not (file-exists-p m-file))
                          (error "File %s does not exist" m-file))
                        (list m-file))
                    (test-matlab-ts-mode-indent-files))))
    (dolist (m-file m-files)
      (let* ((expected-file (replace-regexp-in-string "\\.m$" "_expected.m" m-file))
             (expected (when (file-exists-p expected-file)
                         (with-temp-buffer
                           (insert-file-contents-literally expected-file)
                           (buffer-string)))))

        (save-excursion
          (message "START: test-matlab-ts-mode-indent %s" m-file)
          (find-file m-file)
          (indent-region (point-min) (point-max))
          (trim)
          (let ((got (buffer-substring (point-min) (point-max)))
                (got-file (concat expected-file "~")))
            (set-buffer-modified-p nil)
            (kill-buffer)
            (when (not (string= got expected))
              (let ((coding-system-for-write 'raw-text-unix))
                (write-region got nil got-file))
              (when (not expected)
                (error "Baseline for %s does not exists - if %s looks good rename it to %s"
                       m-file got-file expected-file))
              (error "Baseline for %s does not match, got: %s, expected: %s"
                     m-file got-file expected-file))))

        (when expected ;; expected-file exists?
          (test-matlab-ts-mode-indent--typing m-file expected expected-file)))
          
      (message "PASS: test-matlab-ts-mode-indent %s" m-file)))
  "success")

(provide 'test-matlab-ts-mode-indent)
;;; test-matlab-ts-mode-indent.el ends here
