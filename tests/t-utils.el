;;; t-utils.el --- Test utilities -*- lexical-binding: t -*-
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
;; Test utilities used by test-*.el files.
;;

;;; Code:

(require 'cl-seq)

;; Add abs-path of ".." to load-path so we can require packages from above us.
(let* ((lf (or load-file-name (buffer-file-name (current-buffer))))
       (d1 (file-name-directory lf))
       (parent-dir (expand-file-name (file-name-directory (directory-file-name d1)))))
  (add-to-list 'load-path parent-dir t))

(defun t-utils-trim ()
  "Trim trailing whitespace and lines with utf-8-unix encoding."
  (setq buffer-file-coding-system 'utf-8-unix)
  (let ((delete-trailing-lines t))
    (delete-trailing-whitespace (point-min) (point-max))))

(defun t-utils-get-files (subdir base-regexp &optional skip-regexp file-to-use)
  "Return list of full paths, /path/to/SUBDIR/FILE.
The FILE basenames returned match BASE-REGEXP.
Files matching optional SKIP-REGEXP are ignored.
Optional FILE-TO-USE narrow the list of full paths to that file
and the result is a list of one file.

For example,
  (t-utils-get-files \"test-LANGUAGE-ts-mode-files\"
                     \"*\\.lang$\" \"_expected\\.lang$\" file-to-use)
will return a list of /path/to/test-NAME/*.lang files, skipping
all *_expected.lang files when file-to-use is nil."

  (let ((files (cl-delete-if (lambda (file)
                               (and skip-regexp
                                    (string-match skip-regexp file)))
                             (directory-files subdir t base-regexp))))
    (when file-to-use
      (let ((true-file-to-use (file-truename file-to-use)))
        (when (not (member true-file-to-use files))
          (if (file-exists-p true-file-to-use)
              (error "File %s, resolved to %s, is not a valid selection.
It should be one of %S" file-to-use true-file-to-use files)
            (error "File %s does not exist" file-to-use)))
        (setq files (list true-file-to-use))))
    files))

(defun t-utils-is-treesit-available (language test-name)
  "Is tree-sitter ready for LANGUAGE?
If not available a message saying skipping TEST-NAME is displayed."
  (let ((available (and (>= emacs-major-version 30) ;; treesit package comes with Emacs 30
                        (progn
                          (require 'treesit)
                          (when (fboundp 'treesit-ready-p)
                            (treesit-ready-p language t))))))
    (when (not available)
      (message "skipping-test: %s - %S tree sitter not available." test-name language))
    available))

(defun t-utils-run (&optional match)
  "Run test files in current directory matching regexp, MATCH.
If optional MATCH is non-nil, only run test file names whose
non-directory part matches the regexp, MATCH.  For example,
\"^test-foo.*\\\\.el$\" would run tell t-run to run \"test-foo*.el$\"
files.  The default MATCH is \"^test-.*\\\\.el$\""
  (when (not match)
    (setq match "^test-.*\\.el$"))

  (dolist (test-file (directory-files "." t match))
    (when (not (load-file test-file))
      (error "Failed to load %s" test-file))
    (let ((test-fun (intern
                     (replace-regexp-in-string "\\.el" "" (file-name-nondirectory test-file)))))
      (funcall test-fun))))

(defun t-utils--took (start-time)
  "Return \"- took N seconds\".
N is `current-time' minus START-TIME."
  (format "- took %.2f seconds" (float-time (time-subtract (current-time) start-time))))

(defun t-utils-test-font-lock (test-name lang-files code-to-face)
  "Test font-lock using on each lang-file in LANG-FILES list.
Foreach file in LANG-FILES compare the file against NAME_expected.txt, where
NAME the file name minus the extension.  NAME_expected.txt is of same
length as the file and has a character for each face setup by font-lock.
CODE_TO_FACE is an alist where each elment is (CHAR . FACE).
TEST-NAME is used when displaying messages.

If NAME_expected.txt does not exists or doesn't match the results we
got, a NAME_expected.txt~ will be generated.  After reviewing
NAME_expected.txt~, you should rename it to NAME_expected.txt or fix
your code and rerun the test.

For example, suppose our LANG-FILE contains
    int foo(void) {
        return 1;
    }
our NAME_expected.txt will contain:
    kkk fffDkkkkD b
        kkkkkk nD
    D
where int and void are keywords, etc. and CODE-TO-FACE contains:
  \\='((\"b\" . font-lock-bracket-face)
    (\"d\" . default)
    (\"D\" . font-lock-delimiter-face)
    (\"f\" . font-lock-function-name-face)
    (\"k\" . font-lock-keyword-face)
    (\"n\" . font-lock-constant-face))"

  (let ((face-to-code (mapcar (lambda (pair)
                                (cons (cdr pair) (car pair)))
                              code-to-face)))
    (dolist (lang-file lang-files)
      (save-excursion
        (let ((start-time (current-time)))
          (message "START: %s %s" test-name lang-file)

          (when (boundp 'treesit-font-lock-level)
            (setq treesit-font-lock-level 4))

          (find-file lang-file)

          ;; Force font lock to throw catchable errors.
          (font-lock-mode 1)
          (font-lock-flush (point-min) (point-max))
          (font-lock-ensure (point-min) (point-max))

          (goto-char (point-min))
          (let* ((got "")
                 (expected-file (replace-regexp-in-string "\\.[^.]+$" "_expected.txt"
                                                          lang-file))
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
                  (error "Face, %S, is not in code-to-face alist" face))
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
                       lang-file got-file expected-file))
              (when (= (length got) (length expected))
                (let* ((diff-idx (1- (compare-strings got nil nil expected nil nil)))
                       (got-code (substring got diff-idx (1+ diff-idx)))
                       (got-face (cdr (assoc got-code code-to-face)))
                       (expected-code (substring expected diff-idx (1+ diff-idx)))
                       (expected-face (cdr (assoc expected-code code-to-face))))
                  (error "Baseline for %s does not match, got: %s, expected: %s.  \
Difference at column %d: got code-to-face (\"%s\" . %S), expected code-to-face (\"%s\" . %S)"
                         lang-file got-file expected-file
                         diff-idx
                         got-code got-face
                         expected-code expected-face)))
              (error "Baseline for %s does not match, lengths are different, got: %s, expected: %s"
                     lang-file got-file expected-file))
            (kill-buffer))
          (message "PASS: %s %s %s" test-name lang-file (t-utils--took start-time)))))))

(defun t-utils--test-indent-typing (lang-file lang-file-mode
                                              expected expected-file
                                              &optional line-manipulator)
  "Exercise indent by simulating the creation of LANG-FILE via typing.
This compares the simulation of typing LANG-FILE against the
EXPECTED content in EXPECTED-FILE

The typing occurs in a buffer named \"typing__NAME.EXT\" where NAME.EXT
is the basename of LANG-FILE.

The typing buffer is initialized with the string-trim'd version of the
non-empty lines of LANG-FILE.  If optional LINE-MANIPULATOR function is
specified, it is called with the typing buffer as the current
buffer.  LINE-MANIPULATOR should only adjust whitespace in the lines.  It
should not add newlines to the buffer.  LINE-MANIPULATOR is called from
within a `save-excursion', so your function doesn't need to do that.

After initializating the typing buffer, it's mode is set to
LANG-FILE-MODE.  Each line is then indented via `indent-for-tab-command'
and blank lines are inserted by calling `newline'.`"

  (let* ((typing-lang-file-name (concat "typing__" (file-name-nondirectory lang-file)))
         (contents (with-temp-buffer
                     (insert-file-contents-literally lang-file)
                     (buffer-substring (point-min) (point-max))))
         (lines (split-string (string-trim contents) "\n")))
    (with-current-buffer (get-buffer-create typing-lang-file-name)
      (erase-buffer)
      (funcall lang-file-mode)

      ;; Insert the non-empty lines into typing-lang-file-name buffer
      (dolist (line lines)
        (setq line (string-trim line))
        (when (not (string= line ""))
          (insert line "\n")))

      (goto-char (point-min))

      (when line-manipulator
        (save-excursion
          (funcall line-manipulator)))

      ;; Now indent each line and insert the empty ("") lines into typing-lang-file-buffer
      ;; as we indent. This exercises the RET and TAB behaviors which cause different
      ;; tree-sitter nodes to be provided to the indent engine rules.
      (while (not (eobp))

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

      (t-utils-trim)

      (let ((typing-got (buffer-substring (point-min) (point-max))))
        (set-buffer-modified-p nil)
        (kill-buffer)
        (when (not (string= typing-got expected))
          (let ((coding-system-for-write 'raw-text-unix)
                (typing-got-file (replace-regexp-in-string "\\.\\([^.]+\\)$"
                                                           "_typing.\\1~"
                                                           lang-file)))
            (write-region typing-got nil typing-got-file)
            (error "Typing %s line-by-line does not match %s, we got %s" lang-file expected-file
                   typing-got-file)))))))

(defun t-utils-test-indent (test-name lang-files &optional line-manipulator)
  "Test indent on each file in LANG-FILES list.
Compare indent of each NAME.EXT in LANG-FILES against NAME_expected.EXT.
TEST-NAME is used in messages.

If NAME_expected.EXT does not exist or the indent of NAME.EXT doesn't
match NAME_expected.txt, NAME_expected.EXT~ will be created.  You are
then instructured to validate the indent and rename NAME_expected.EXT~
to NAME_expected.EXT.

To add a test for TEST-NAME.el, in it's corresponding TEST-NAME-files/
directory, create TEST-NAME-files/NAME.EXT, then run the test.  Follow
the messages to accept the generated baseline after validating it.

Two methods are used to indent each file in LANG-FILES,
 1. (indent-region (point-min) (point-man))
 2. Simulation of typing lang-file to exercise TAB and RET,
    see `t-utils--test-indent-typing'.  In tree-sitter modes, TAB and RET
    need to be handled and this verifies they are handled.

See `t-utils--test-indent-type' for LINE-MANIPULATOR."

  (dolist (lang-file lang-files)
    (let* ((expected-file (replace-regexp-in-string "\\.\\([^.]+\\)$" "_expected.\\1" lang-file))
           (expected (when (file-exists-p expected-file)
                       (with-temp-buffer
                         (insert-file-contents-literally expected-file)
                         (buffer-string))))
           lang-file-major-mode)
      
      ;; Indent lang-file
      (save-excursion
        (let ((start-time (current-time)))
          (message "START: %s <indent-region> %s" test-name lang-file)
          (find-file lang-file)
          (setq lang-file-major-mode major-mode)
          (indent-region (point-min) (point-max))
          (t-utils-trim)
          (let ((got (buffer-substring (point-min) (point-max)))
                (got-file (concat expected-file "~")))
            (set-buffer-modified-p nil)
            (kill-buffer)
            (when (not (string= got expected))
              (let ((coding-system-for-write 'raw-text-unix))
                (write-region got nil got-file))
              (when (not expected)
                (error "Baseline for %s does not exists - if %s looks good rename it to %s"
                       lang-file got-file expected-file))
              (error "Baseline for %s does not match, got: %s, expected: %s"
                     lang-file got-file expected-file)))
          (message "PASS: %s <indent-region> %s %s" test-name lang-file
                   (t-utils--took start-time))))

      ;; Now, simulate typing lang-file and indent it (exercise TAB and RET)
      (let ((start-time (current-time)))
        (message "START: %s <indent-via-typing> %s" test-name lang-file)
        (t-utils--test-indent-typing lang-file lang-file-major-mode
                                     expected expected-file
                                     line-manipulator)
        (message "PASS: %s <indent-via-typing> %s %s" test-name lang-file
                 (t-utils--took start-time))))))

(defun t-utils-test-syntax-table (test-name lang-files)
  "Test syntax-table on each file in LANG-FILES list.
Compare syntax-table of each NAME.EXT in LANG-FILES against NAME_expected.txt.
TEST-NAME is used in messages.

If NAME_expected.txt does not exist or the syntax-table of NAME.txt doesn't
match NAME_expected.txt, NAME_expected.txt~ will be created.  You are
then instructured to validate the syntax-table and rename NAME_expected.txt~
to NAME_expected.txt.

To add a test for TEST-NAME.el, in it's corresponding TEST-NAME-files/
directory, create TEST-NAME-files/NAME.EXT, then run the test.  Follow
the messages to accept the generated baseline after validating it."

  (dolist (lang-file lang-files)
    (save-excursion
      (let ((start-time (current-time)))
        (message "START: %s %s" test-name lang-file)

        (find-file lang-file)
        (goto-char (point-min))

        (let* ((got "")
               (expected-file (replace-regexp-in-string "\\.[^.]+$" "_expected.txt" lang-file))
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
                     lang-file got-file expected-file))
            (error "Baseline for %s does not match, got: %s, expected: %s"
                   lang-file got-file expected-file))
          (kill-buffer))
        (message "PASS: %s %s %s" test-name lang-file (t-utils--took start-time))))))

(provide 't-utils)
;;; t-utils.el ends here
