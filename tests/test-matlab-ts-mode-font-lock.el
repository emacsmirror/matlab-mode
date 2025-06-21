;;; test-matlab-ts-mode-font-lock.el --- Test matlab-ts-mode font-lock -*- lexical-binding: t -*-
;;
;; Copyright Free Software Foundation

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
;; Validate matlab-ts-mode font-lock faces.
;; Load ../matlab-ts-mode.el via require and run font-lock tests using
;; ./test-matlab-ts-mode-font-lock-files/NAME.m comparing against
;; ./test-matlab-ts-mode-font-lock-files/NAME_expected.txt
;;

;;; Code:

(require 'cl-macs)

;; Add abs-path of ".." to load-path so we can (require 'matlab-ts-mode)
(let* ((lf (or load-file-name (buffer-file-name (current-buffer))))
       (d1 (file-name-directory lf))
       (parent-dir (expand-file-name (file-name-directory (directory-file-name d1)))))
  (add-to-list 'load-path parent-dir t))

(require 'matlab-ts-mode)

(defun test-matlab-ts-mode-font-lock-files ()
  "Return list of full paths to each test-matlab-ts-mode-font-lock-files/*.m."
  (directory-files "test-matlab-ts-mode-font-lock-files" t "\\.m$"))

(defvar test-matlab-ts-mode-font-lock
  (cons "test-matlab-ts-mode-font-lock" (test-matlab-ts-mode-font-lock-files)))

(cl-defun test-matlab-ts-mode-font-lock (&optional m-file)
  "Test font-lock using ./test-matlab-ts-mode-font-lock-files/NAME.m.
Compare ./test-matlab-ts-mode-font-lock-files/NAME.m against
./test-matlab-ts-mode-font-lock-files/NAME_expected.txt, where
NAME_expected.txt is of same length as NAME.m and has a character for
each face setup by font-lock.

If M-FILE NAME.m is not provided, loop comparing all
./test-matlab-ts-mode-font-lock-files/NAME.m files.

For example, given foo.m containing:
    function a = foo
        a = 1;
    end
we'll have expected that looks like:
    kkkkkkkk v d fff
        d d dd
    kkk

For debugging, you can run with a specified NAME.m,
  M-: (test-matlab-ts-mode-font-lock \"test-matlab-ts-mode-font-lock-files/NAME.m\")"

  (when (or (< emacs-major-version 30)
            (not (progn
                   (require 'treesit)
                   (when (fboundp 'treesit-ready-p)
                     (treesit-ready-p 'matlab t)))))
    (message "skipping-test: test-matlab-ts-mode-font-lock.el - matlab tree sitter not available.")
    (cl-return-from test-matlab-ts-mode-font-lock))

  (let* ((m-files (if m-file
                      (progn
                        (setq m-file (file-truename m-file))
                        (when (not (file-exists-p m-file))
                          (error "File %s does not exist" m-file))
                        (list m-file))
                    (test-matlab-ts-mode-font-lock-files)))
         (code-to-face '(
                         ("b" . font-lock-bracket-face)
                         ("B" . font-lock-builtin-face)
                         ("c" . font-lock-comment-face)
                         ("C" . font-lock-comment-delimiter-face)
                         ("d" . default)
                         ("D" . font-lock-delimiter-face)
                         ("f" . font-lock-function-name-face)
                         ("h" . font-lock-doc-face) ;; function doc help comment
                         ("H" . matlab-ts-comment-heading-face) ;; %% comment heading
                         ("k" . font-lock-keyword-face)
                         ("n" . font-lock-constant-face) ;; numbers
                         ("s" . font-lock-string-face)
                         ("S" . matlab-ts-string-delimiter-face)
                         ("p" . matlab-ts-pragma-face)
                         ("P" . font-lock-property-name-face)
                         ("t" . font-lock-type-face)
                         ("v" . font-lock-variable-name-face)
                         ("w" . font-lock-warning-face)
                         ))
         (face-to-code (mapcar (lambda (pair)
                                 (cons (cdr pair) (car pair)))
                               code-to-face)))
    (dolist (m-file m-files)
      (save-excursion
        (message "START: test-matlab-ts-mode-font-lock %s" m-file)

        (when (boundp 'treesit-font-lock-level)
          (setq treesit-font-lock-level 4))

        (find-file m-file)

        ;; Force font lock to throw catchable errors.
        (font-lock-mode 1)
        (font-lock-flush (point-min) (point-max))
        (font-lock-ensure (point-min) (point-max))

        (goto-char (point-min))
        (let* ((got "")
               (expected-file (replace-regexp-in-string "\\.m$" "_expected.txt"
                                                        m-file))
               (got-file (concat expected-file "~"))
               (expected (when (file-exists-p expected-file)
                           (with-temp-buffer
                             (insert-file-contents-literally expected-file)
                             (buffer-string)))))
          (while (not (eobp))
            (let* ((face (if (face-at-point) (face-at-point) 'default))
                   (code (if (looking-at "\\([ \t\n]\\)")
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
              (error "Baseline for %s does not exists.  \
See %s and if it looks good rename it to %s"
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
      (message "PASS: test-matlab-ts-mode-font-lock %s" m-file)))
  "success")

(provide 'test-matlab-ts-mode-font-lock)
;;; test-matlab-ts-mode-font-lock.el ends here
