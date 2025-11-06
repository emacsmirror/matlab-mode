;;; test-matlab-ts-mode-parser.el --- -*- lexical-binding: t -*-

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
;; Validate matlab-ts-mode indent.
;; Load ../matlab-ts-mode.el via require and run indent tests using
;; ./test-matlab-ts-mode-parser-files/NAME.m comparing against
;; ./test-matlab-ts-mode-parser-files/NAME_expected.m
;;

;;; Code:

(require 'cl-macs)
(require 'cl-seq)

(require 't-utils)
(require 'matlab-ts-mode)

(defvar test-matlab-ts-mode-parser--current-parser-level nil)

(defvar test-matlab-ts-mode-parser--file nil)

(defun test-matlab-ts-mode-parser--file (m-file)
  "Test an individual M-FILE.
This is provided for debugging.
  M-: (test-matlab-ts-mode-parser--file \"test-matlab-ts-mode-parser-files/M-FILE\")"
  (let ((test-matlab-ts-mode-parser--file m-file))
    (ert-run-tests-interactively "test-matlab-ts-mode-parser")))

(ert-deftest test-matlab-ts-mode-parser ()
  "Test matlab tree-sitter parser on ./test-matlab-ts-mode-parser-files/NAME.m.
Compare parse tree of ./test-matlab-ts-mode-parser-files/NAME.m against
./test-matlab-ts-mode-parser-files/NAME_expected.txt.  This loops
on all ./test-matlab-ts-mode-parser-files/NAME.m files.

To add a test, create
  ./test-matlab-ts-mode-parser-files/NAME.m
and run this function.  The baseline is saved for you as
  ./test-matlab-ts-mode-parser-files/NAME_expected.txt~
after validating it, rename it to
  ./test-matlab-ts-mode-parser-files/NAME_expected.txt

To understand the format of NAME_expected.txt files, see
`t-utils-view-parse-tree'."

  (should (equal (test-matlab-ts-mode-parser--all-files) ""))

  (let* ((test-name "test-matlab-ts-mode-parser")
         (m-files (t-utils-get-files test-name
                                     :recursively t
                                     :base-regexp (rx ".m" eos)
                                     :file-to-use test-matlab-ts-mode-parser--file)))
    (t-utils-error-if-no-treesit-for 'matlab test-name)
    (t-utils-test-parser test-name m-files)))

(defun test-matlab-ts-mode-parser--use-file (m-file)
  "Should we be using M-FILE not in as a test point?
M-FILE is not in test-matlab-ts-mode-parser-files"
  (and
   ;; Must be one of the test-matlab-ts-mode-**.m files
   (string-match-p "^\\./test-matlab-ts-mode-" m-file)
   ;; Skip ourself
   (not (string-match-p "^\\./test-matlab-ts-mode-parser-files" m-file))
   ;; Skip corrupted files
   (not (string-match-p "^\\./test-matlab-ts-mode-file-encoding-files" m-file))
   ;; Skip test-matlab-ts-mode-indent-xr-files because they are generated on-the-fly
   (not (string-match-p "^\\./test-matlab-ts-mode-indent-xr-files" m-file))
   ;; Skip expected baselines because these are effectively duplicates
   (not (string-match-p "_expected\\(?:_msgs\\)?\\.m$" m-file))
   ;; Skip any *.m file that has a corresponding *.skip.txt file because that has
   ;; some open issue that needs resolving
   (let ((skip-file (replace-regexp-in-string "\\.m$" ".skip.txt" m-file)))
     (not (file-exists-p skip-file)))))

(defvar test-matlab-ts-mode-parser--file-ht nil)

(defun test-matlab-ts-mode-parser--update-file (m-file)
  "Do we need to copy M-FILE into ./test-matlab-mode-parser-files/?"
  (let ((test-m-file (replace-regexp-in-string "^\\./\\([^/]+/\\)"
                                               "./test-matlab-ts-mode-parser-files/copy-of-\\1"
                                               m-file))
        update)

    (cl-assert (not (equal test-m-file m-file)))

    (let ((other-m-file (gethash test-m-file test-matlab-ts-mode-parser--file-ht)))
      (when other-m-file
        (error "Files \"%s\" and \"%s\" map to the same file \"%s\""
               other-m-file
               m-file
               test-m-file))
      (puthash test-m-file m-file test-matlab-ts-mode-parser--file-ht))

    (if (file-exists-p test-m-file)
        (let ((m-file-contents (with-temp-buffer
                                 (insert-file-contents-literally m-file)
                                 (buffer-string)))
              (test-m-file-contents (with-temp-buffer
                                      (insert-file-contents-literally test-m-file)
                                      (buffer-string))))
          (setq update (not (string= m-file-contents test-m-file-contents))))
      (setq update t))

    (list update m-file test-m-file)))

;; TODO update the files and add ert test to validate (test-matlab-ts-mode-parser--all-files)
;; returns ""

(defun test-matlab-ts-mode-parser--all-files (&optional update-if-needed)
  "Verify we use ./test-matlab-ts-mode-*/*.m as baselines.
We use all ./test-matlab-ts-mode-*/*.m files as test points.

If UPDATE-IF-NEEDED is t, update the file by copying it, else return a
string which is empty if no-updates needed, otherwise the string tells
you that updates are needed."

  (let* ((me (symbol-file 'test-matlab-ts-mode-parser--all-files))
         (my-dir (file-name-directory me))
         (default-directory my-dir))

    (setq test-matlab-ts-mode-parser--file-ht (make-hash-table :test #'equal))

    (let* ((all-m-files (cl-delete-if (lambda (m-file)
                                        (let ((use (test-matlab-ts-mode-parser--use-file m-file)))
                                          (not use)))
                                      (directory-files-recursively "." (rx ".m" eos))))
           (copy-of-m-files (cl-delete-if (lambda (m-file)
                                            (not (string-match-p
                                                  "^./test-matlab-ts-mode-parser-files/copy-of-"
                                                  m-file)))
                                          (directory-files-recursively
                                           "./test-matlab-ts-mode-parser-files" (rx ".m" eos))))
           (updates (mapcar #'test-matlab-ts-mode-parser--update-file all-m-files))
           (n-updates 0))

      (dolist (tuple updates)
        (let ((update (nth 0 tuple))
              (m-file (nth 1 tuple))
              (test-m-file (nth 2 tuple)))
          (when update
            (setq n-updates (1+ n-updates))
            (when update-if-needed
              (let ((dir (file-name-directory test-m-file)))
                (when (not (file-directory-p dir))
                  (make-directory dir t)))
              (copy-file m-file test-m-file t)))))

      (dolist (copy-of-m-file copy-of-m-files)
        (when (not (gethash copy-of-m-file test-matlab-ts-mode-parser--file-ht))
          (setq n-updates (1+ n-updates))
          (when update-if-needed
            (delete-file copy-of-m-file))
          (let ((expected (replace-regexp-in-string "\\.m$" "_expected.txt" copy-of-m-file)))
            (when (file-exists-p expected)
              (delete-file expected)
              (when update-if-needed
                expected)))))

      (if update-if-needed
          (format "%d file(s) updated" n-updates)
        (if (= n-updates 0)
            ""
          (format (concat "%d ./test-matlab-ts-mode-parser-files/*.m require updating, run  "
                          "M-: (test-matlab-ts-mode-parser--all-files t)  "
                          "to update them")
                  n-updates))))))

(provide 'test-matlab-ts-mode-parser)
;;; test-matlab-ts-mode-parser.el ends here

;; LocalWords:  utils defun eos treesit gethash puthash setq mapcar dolist xr
