;;; metest-font-lock-test2.el --- Testing suite for MATLAB Emacs -*- lexical-binding: t -*-
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
;; Tests to exercise font-lock using ./metest-font-lock-test2-files/*
;;

;;; Code:

(defun metest-font-lock-test2-files ()
  "Return list of full paths to each metest-font-lock-test2-files/*.m."
  (directory-files "metest-font-lock-test2-files" t "\\.m$"))

(defvar metest-font-lock-test2 (cons "font-lock-test2" (metest-font-lock-test2-files)))

(defun metest-font-lock-test2 (&optional m-file)
  "Test font-lock using ./metest-font-lock-test2-files/M-FILE.
Compare ./metest-font-lock-test2-files/M-FILE against
./metest-font-lock-test2-files/NAME_expected.txt, where NAME_expected.txt is of
same length as M-FILE and has a character for each face setup by
font-lock.

If M-FILE is not provided, loop comparing all
  ./metest-font-lock-test2-files/*.m

For example, given foo.m containing
    function a = foo
        a = 1;
    end
we'll have expected that looks like
    kkkkkkkk v d fff
        d d dd
    kkk

For debugging, you can run with a specified M-FILE,
  M-: (metest-font-lock-test2 \"metest-font-lock-test2-files/M-FILE\")"
  (let* ((m-files (if m-file
                      (progn
                        (setq m-file (file-truename m-file))
                        (when (not (file-exists-p m-file))
                          (error "File %s does not exist" m-file))
                        (list m-file))
                    (metest-font-lock-test2-files)))
         (code-to-face '(
                         ("c" . font-lock-comment-face)
                         ("C" . font-lock-comment-delimiter-face)
                         ("d" . default)
                         ("f" . font-lock-function-name-face)
                         ("k" . font-lock-keyword-face)
                         ("t" . font-lock-type-face)
                         ("v" . font-lock-variable-name-face)
                         ))
         (face-to-code (mapcar (lambda (pair)
                                 (cons (cdr pair) (car pair)))
                               code-to-face)))
    (dolist (m-file m-files)
      (save-excursion
        (message "START: metest-font-lock-test2 %s" m-file)

        (find-file m-file)

        ;; Force font lock to throw catchable errors.
        (font-lock-mode 1)
        (font-lock-flush (point-min) (point-max))
        (font-lock-ensure (point-min) (point-max))
        (font-lock-fontify-region (point-min) (point-max))
        
        (goto-char (point-min))
        (let* ((got "")
               (expected-file (replace-regexp-in-string "\\.m$" "_expected.txt" m-file))
               (got-file (concat expected-file "~"))
               (expected (when (file-exists-p expected-file)
                           (with-temp-buffer
                             (insert-file-contents-literally expected-file)
                             (buffer-string)))))
          (while (not (eobp))
            (let* ((face (if (face-at-point) (face-at-point) 'default))
                   (code (if (looking-at "\\([ \t]\\)")
                             (match-string 1)
                           (cdr (assoc face face-to-code)))))
              (when (not code)
                (error "Face, %S, is not in face-to-code alist" face))
              (setq got (concat got code))
              (forward-char)
              (when (looking-at "\n")
                (setq got (concat got "\n"))
                (forward-char))))

          (when (not (string= got expected))
            (let ((coding-system-for-write 'raw-text-unix))
              (write-region got nil got-file))
            (when (not expected)
              (error "Baseline for %s does not exists.  See %s and if it looks good rename it to %s"
                     m-file got-file expected-file))
            (when (= (length got) (length expected))
              (let* ((diff-idx (1- (compare-strings got nil nil expected nil nil)))
                     (got-code (substring got diff-idx (1+ diff-idx)))
                     (got-face (cdr (assoc got-code code-to-face)))
                     (expected-code (substring expected diff-idx (1+ diff-idx)))
                     (expected-face (cdr (assoc expected-code code-to-face))))
                (error "Baseline for %s does not match, got: %s, expected: %s.  \
Difference at column %d (got code-to-face \"%s\" . %S, expected code-to-face \"%s\" . %S"
                       m-file got-file expected-file
                       diff-idx
                       got-code got-face
                       expected-code expected-face)))
            (error "Baseline for %s does not match, lengths are different, got: %s, expected: %s"
                   m-file got-file expected-file))
          (kill-buffer)))
      (message "PASS: metest-font-lock-test2 %s" m-file)))
  "success")

(provide 'metest-font-lock-test2)
;;; metest-font-lock-test2.el ends here
