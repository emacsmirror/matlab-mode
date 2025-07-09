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

(require 'cl-macs)
(require 'cl-seq)
(require 'diff)
(require 'outline)
(require 'treesit)

;; Add abs-path of ".." to load-path so we can require packages from above us.
(let* ((lf (or load-file-name (buffer-file-name (current-buffer))))
       (d1 (file-name-directory lf))
       (parent-dir (expand-file-name (file-name-directory (directory-file-name d1)))))
  (add-to-list 'load-path parent-dir t))

(defun t-utils--trim ()
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

(defvar-local t-utils--buf-file nil
  "Name of file associated with t-utils temporary buffers.")

(defun t-utils--insert-file-for-test (file)
  "Insert FILE into current temporary buffer for testing."
  (insert-file-contents-literally file)
  ;; CRLF -> LF for consistency between Unix and Windows
  (goto-char (point-min))
  (while (re-search-forward "\r" nil t)
    (replace-match ""))
  (goto-char (point-min))
  ;; Set mode
  (when (and (not (looking-at "^.* -\\*-[ \t]+\\([-a-z0-9]+\\)[ \t]+-\\*-"))
             (not (looking-at "^.* -\\*-.*mode:[ \t]+\\([-a-z0-9]+\\).*-\\*-")))
    (error "First line of %s must contain -*- MODE-NAME -*- (or -*- mode: MODE-NAME -*-)" file))
  (let* ((mode (match-string 1))
         (mode-cmd (intern (concat mode "-mode"))))
    (funcall mode-cmd))
  ;; Stash away the real buffer file for later use.
  (setq-local t-utils--buf-file file)
  ;; Incase the mode moves the point, reset to point-min.
  (goto-char (point-min)))

(defun t-utils--took (start-time)
  "Return \"- took N seconds\".
N is `current-time' minus START-TIME."
  (format "- took %.2f seconds" (float-time (time-subtract (current-time) start-time))))

(defun t-utils--get-buf-file ()
  "Return the file corresponding to the buffer under test."
  (cond ((and (local-variable-if-set-p 't-utils--buf-file)
              t-utils--buf-file)
         t-utils--buf-file)
        ((buffer-file-name)
         (buffer-file-name))
        (t
         (error "This must be invoked from within a file buffer"))))

(defun t-utils--diff-strings-impl (start-contents end-contents)
  "Implementation for `t-utils-diff-string'.
Returns diff of START-CONTENTS and END-CONTENTS."
  (with-temp-buffer
    (let* ((tmp-name-prefix (condition-case nil
                                (t-utils--get-buf-file)
                              ( error "t-utils--tmp-file")))
           (start-tmp-file (make-temp-file (concat tmp-name-prefix ".start.") nil ".txt"
                                           start-contents))
           (end-tmp-file (make-temp-file (concat tmp-name-prefix ".end.") nil ".txt"
                                         end-contents))
           (diff-buf (current-buffer)))

      (diff-no-select start-tmp-file end-tmp-file "-u" t diff-buf)

      (read-only-mode -1)

      ;; Delete the "diff -u start-file end-file" command
      (goto-char (point-min))
      (re-search-forward "^--- ")
      (beginning-of-line)
      (delete-region (point-min) (point))

      ;; Remove temp file names and time stamps to make output stable and easier to read
      (re-search-forward "^--- .+$")
      (replace-match "--- start_contents")
      (re-search-forward "^\\+\\+\\+ .+$")
      (replace-match "+++ end_contents")

      ;; Remove the diff finished buffer info. At end of buffer there's a blank line then
      ;; "Diff finished. TIME"
      (goto-char (point-max))
      (forward-line -2)
      (delete-region (point) (point-max))

      (delete-file start-tmp-file)
      (delete-file end-tmp-file)

      (let ((diff-result (buffer-substring-no-properties (point-min) (point-max))))
        (kill-buffer diff-buf)
        diff-result))))

(defvar t-utils--diff-checked nil)

(defun t-utils--diff-check ()
  "Validate diff is setup correctly."

  (let* ((s1 (concat "L1\n" "L2\n" "L3\n" "L4\n" "L5\n" "L6\n" "L7\n" "L8\n" "L9\n" "L10\n"))
         (s2 (replace-regexp-in-string "L5" "L5-MODIFIED" s1))
         (got (t-utils--diff-strings-impl s1 s2))
         (expected (concat "--- start_contents\n"
                           "+++ end_contents\n"
                           "@@ -2,7 +2,7 @@\n"
                           " L2\n L3\n L4\n-L5\n+L5-MODIFIED\n L6\n L7\n L8\n")))
    (when (not (string= got expected))
      (error "Running diff produced unexecpted results.
Verify that diff is setup correctly, check `diff-command', etc.
You can run `t-utils--diff-check' to debug"))))

(defun t-utils-diff-strings (start-contents end-contents)
  "Return diff of START-CONTENTS and END-CONTENTS."

  ;; Do a one time diff on sample start/end contents vs expected result
  (when (not t-utils--diff-checked)
    (t-utils--diff-check)
    (setq t-utils--diff-checked t))

  (t-utils--diff-strings-impl start-contents end-contents))

(defun t-utils--get-point-for-display (point)
  "Return \"<point> L<num> C<num>\" for POINT."
  (format "%d L%d C%d"
          point
          (line-number-at-pos point)
          (save-excursion (goto-char point)
                          (current-column))))

(defvar t-utils--xr-impl-result-active)
(defvar t-utils--xr-impl-result)

(defun  t-utils--use-xr-impl-result ()
  "Send result to `t-utils--xr-impl-result'?"
  (and (boundp 't-utils--xr-impl-result-active)
       t-utils--xr-impl-result-active))

(defun t-utils--xr-impl (commands)
  "Implementation for `t-utils-xr' that processes COMMANDS."
  (when (or (= (point) 1)
            (not (save-excursion (goto-char (1- (point))) (looking-at ")"))))
    (error "Expected point to be after a closing parenthisis, \")\""))

  (let* ((line-move-visual nil) ;; C-n moves by true lines and not the width
         (buf-file (t-utils--get-buf-file))
         (start-line (line-number-at-pos))
         (xr-end-point (point))
         (xr-start-point
          (save-excursion
            (backward-list)
            (when (not (looking-at "(t-utils-xr"))
              (error "`backward-list from point, %d, didn't not jump to (t-utils-xr" xr-end-point))
            (point)))
         (xr-cmd (buffer-substring-no-properties xr-start-point xr-end-point))
         (result (format "\n* Executing commands from %s:%d:%d:\n\n  %s\n"
                         (file-name-nondirectory buf-file)
                         (line-number-at-pos xr-start-point)
                         (save-excursion (goto-char xr-start-point)
                                         (current-column))
                         xr-cmd))
         (cmd-num 0))

    ;; Enable "C-SPC" in `t-utils-xr' commands.  Under regular running, we are being invoked from
    ;; `t-utils-xr-test' and current buffer is a temporary buffer.  In batch mode,
    ;; `transient-mark-mode' is not active, thus activate it.
    (transient-mark-mode 1)

    (dolist (command commands)
      (setq cmd-num (1+ cmd-num))
      (let ((standard-output (generate-new-buffer " *temp t-utils-xr-capture*" t)))
        (unwind-protect
            (let* ((start-pt (point))
                   (start-pt-str (t-utils--get-point-for-display start-pt))
                   (start-contents (buffer-substring-no-properties (point-min) (point-max)))
                   (key-command (when (eq (type-of command) 'string)
                                  ;; Keybinding, e.g. (t-utils-xr "C-M-a")
                                  (let ((cmd (key-binding (kbd command))))
                                    (when (not cmd)
                                      (user-error "%s:%d: Command, %s, is not a known keybinding"
                                                  buf-file start-line command))
                                    cmd))))
              (setq result (concat result "\n"
                                   (format "- Invoking      : %S%s\n"
                                           command (if key-command
                                                       (concat " = " (symbol-name key-command))
                                                     ""))
                                   (format "  Start point   : %4d\n" start-pt)))

              (if key-command
                  ;; a keybinding: (t-util-xr "C-M-a")
                  (call-interactively key-command)
                ;; a command: (t-utils-xr (beginning-of-defun))
                (eval command))

              (let ((end-pt (point))
                    (end-contents (buffer-substring-no-properties (point-min) (point-max)))
                    (debug-msg (format "%d: %S, start point %s" cmd-num command start-pt-str)))

                ;; Record point movement by adding what happened to result
                (if (equal start-pt end-pt)
                    (setq result (concat result "  No point movement\n")
                          debug-msg (concat debug-msg ", no point movement"))
                  (let* ((current-line (buffer-substring-no-properties (line-beginning-position)
                                                                       (line-end-position)))
                         (position (format "%d:%d: " (line-number-at-pos) (current-column)))
                         (carrot (concat (make-string (+ (length position) (current-column)) ?\s)
                                         "^")))
                    (setq result (concat result (format "  Moved to point: %4d\n  : %s%s\n  : %s\n"
                                                        end-pt position current-line carrot))
                          debug-msg (concat debug-msg
                                            (format ", moved point to %s"
                                                    (t-utils--get-point-for-display (point)))))))

                ;; Grab standard-output from `prin1' or `print'
                (with-current-buffer standard-output
                  (let ((contents (string-trim (buffer-substring (point-min) (point-max)))))
                    (when (not (string= contents ""))
                      (setq result (concat result
                                           "  standard-output:\n  "
                                           (replace-regexp-in-string "^" "  " contents)
                                           "\n")))))

                ;; Record buffer modifications by adding what happened to result
                (if (equal start-contents end-contents)
                    (setq result (concat result "  No buffer modifications\n")
                          debug-msg (concat debug-msg ", no buffer modifications"))
                  (setq result (concat result
                                       "  Buffer modified:\n"
                                       "  #+begin_src diff\n"
                                       (t-utils-diff-strings start-contents end-contents)
                                       "  #+end_src diff\n")
                        debug-msg (concat debug-msg ", buffer modified")))

                (when (not (t-utils--use-xr-impl-result))
                  ;; Display debugging info for interactive evaluation of (t-utils-xr COMMANDS)
                  (read-string (concat debug-msg "\n" "Enter to continue:")))))
          ;; unwind-protect unwindforms
          (and (buffer-name standard-output)
               (kill-buffer standard-output)))))

    (if (t-utils--use-xr-impl-result)
        (progn
          (setq t-utils--xr-impl-result result)
          nil)
      result)))

(cl-defmacro t-utils-xr (&rest commands)
  "Execute and record results of each command in list of COMMANDS.
This returns a string recofrding point movement and buffer modification
differences for each command.  See `t-utils-test-xr' for details."
  (t-utils--xr-impl commands))

(defun t-utils--eval-sexp-print-advice (_old-function &rest _)
  "Advice around `elisp--eval-last-sexp-print-value' to ignore the print.")

(defun t-utils-test-xr (test-name lang-files)
  "Execute and record (t-utils-xr COMMANDS) from LANG-FILES list.
For each NAME.EXT in LANG-FILES, run each (t-utils-xr COMMANDS) and
compare results against NAME_expected.org.  TEST-NAME is used in
messages.

The commands that you can place within (t-utils-xr COMMANDS) are
 1. Lisp expressions.  For example,
      (t-utils-xr (beginning-of-defun))
 2. Keybindings.  For example,
      (t-utils-xr \"C-M-a\")
 3. `standard-output' is captured.  You use (prin1 OBJECT) or (print OBJECT)
    to write `standard-output', which lets you capture the results
    of functions in the baseline.  For example,
      (t-utils-xr (prin1 (a-buffer-query-function-special-to-your-mode)))
Multiple expressions or keybindings can be specified.

Consider ./test-defun-movement/my_test.c:

  1 | #include <stdlib.h>
  2 |
  3 | int fcn1(void) {
  4 |   // (t-utils-xr \"C-M-e\" \"C-M-e\")
  5 |   return 1;
  6 | }
  7 |
  8 | int main(void) {
  9 |   return fcn1();
  10|   // (t-utils-xr (beginning-of-defun) (beginning-of-defun))
  11| }

You can interactively evaulate each (t-utils-xr COMMANDS) by placing the
`point' on the closing parenthesis and typing \\[eval-last-sexp].  For
example, with the point after the closing parenthesis on line 4 and
running \\[eval-last-sexp], we'll see in the *Messages* buffer:

    * Executing commands from my_test.c:4:

      // (t-utils-xr \"C-M-e\" \"C-M-e\")

    - Invoking      : \"C-M-e\" = c-end-of-defun
      Start point   :   72
      Moved to point:   87
      : 7:0:
      :      ^
      No buffer modifications

    - Invoking      : \"C-M-e\" = c-end-of-defun
      Start point   :   87
      Moved to point:  158
      : 12:0:
      :       ^
      No buffer modifications

Running

  M-: (t-utils-test-xr \"test-defun-movement\"
                       \\='(\"test-defun-movement/my_test.c\"))

will run the two (t-utils-xr COMMANDS) statements from line 4 and 10 of
my_test.c.  The result is compared against
test-defun-movement/my_test_expected.org.  If my_test_expected.org does
not exist or result doesn't match the existing my_test_expected.org,
my_test_expected.org~ is generated and if it looks correct, you should
rename it to my_test_expected.org.  The contents of my_test_expected.org
for this example is:

  TODO xxx contents of my_test_expected.org"

  (dolist (lang-file lang-files)
    (with-temp-buffer
      (t-utils--insert-file-for-test lang-file)
      (let* ((start-time (current-time))
             (expected-file (replace-regexp-in-string "\\.[^.]+$" "_expected.org"
                                                      lang-file))
             (expected (when (file-exists-p expected-file)
                         (with-temp-buffer
                           (insert-file-contents-literally expected-file)
                           (buffer-string))))
             (got "#+startup: showall\n")
             (got-file (concat expected-file "~")))

        (message "START: %s %s" test-name lang-file)

        (while (re-search-forward "(t-utils-xr" nil t)
          (re-search-backward "(")
          (forward-list)
          (let* ((xr-end-point (point)))
            (setq t-utils--xr-impl-result-active t)
            (unwind-protect
                (progn
                  ;; `eval-last-sexp' on (t-utils-xr COMMANDS) calls
                  ;; `elisp--eval-last-sexp-print-value' which will (prin1 value output) where
                  ;; output is t which means send to echo area.  We don't want to print value which
                  ;; is nil in our case, so we override elisp--eval-last-sexp-print-value locally
                  ;; during this eval.
                  (advice-add #'elisp--eval-last-sexp-print-value :override
                              #'t-utils--eval-sexp-print-advice)

                  (eval-last-sexp nil)
                  (setq got (concat got t-utils--xr-impl-result)
                        t-utils--xr-impl-result-active nil
                        t-utils--xr-impl-result nil))
              (progn
                (setq t-utils--xr-impl-result-active nil
                      t-utils--xr-impl-result nil)
                (advice-remove #'elisp--eval-last-sexp-print-value
                              #'t-utils--eval-sexp-print-advice)))

            ;; look for next (t-utils-xr COMMANDS)
            (goto-char xr-end-point)))

        (kill-buffer)

        (when (string= got "")
          (error "No (t-utils-xr COMMANDS) found in %s" lang-file))

        (when (not (string= got expected))
          (let ((coding-system-for-write 'raw-text-unix))
            (write-region got nil got-file))
          (when (not expected)
            (error "Baseline for %s does not exists.  \
See %s and if it looks good rename it to %s"
                   lang-file got-file expected-file))
          (error "Baseline for %s does not match, got: %s, expected: %s"
                 lang-file got-file expected-file))

        (message "PASS: %s %s %s" test-name lang-file (t-utils--took start-time))))))

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
    (\"n\" . font-lock-constant-face))

xxx give example calling test-name.el (and for others)"

  (let ((face-to-code (mapcar (lambda (pair)
                                (cons (cdr pair) (car pair)))
                              code-to-face)))
    (dolist (lang-file lang-files)
      (with-temp-buffer
        (t-utils--insert-file-for-test lang-file)
        (let ((start-time (current-time)))
          (message "START: %s %s" test-name lang-file)

          ;; Force font lock to throw catchable errors.
          (font-lock-mode 1)
          (font-lock-flush (point-min) (point-max))
          (font-lock-ensure (point-min) (point-max))

          (goto-char (point-min))
          (let* ((expected-file (replace-regexp-in-string "\\.[^.]+$" "_expected.txt"
                                                          lang-file))
                 (expected (when (file-exists-p expected-file)
                             (with-temp-buffer
                               (insert-file-contents-literally expected-file)
                               (buffer-string))))
                 (got "")
                 (got-file (concat expected-file "~")))

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

            (kill-buffer)

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
                     lang-file got-file expected-file)))
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

      (t-utils--trim)

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

To add a test for TEST-NAME.el which call this function, in the
corresponding TEST-NAME-files/ directory, create
TEST-NAME-files/NAME.EXT, then run the test.  Follow the messages to
accept the generated baseline after validating it.

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
      (with-temp-buffer
        (let ((start-time (current-time)))
          (t-utils--insert-file-for-test lang-file)
          (message "START: %s <indent-region> %s" test-name lang-file)
          (setq lang-file-major-mode major-mode)
          (indent-region (point-min) (point-max))
          (t-utils--trim)
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

If NAME_expected.txt does not exist or the result of NAME.ext doesn't
match NAME_expected.txt, NAME_expected.txt~ will be created.  You are
then instructured to validate the result and rename NAME_expected.txt~
to NAME_expected.txt.

To add a test for TEST-NAME.el which call this function, in the
corresponding TEST-NAME-files/ directory, create
TEST-NAME-files/NAME.EXT, then run the test.  Follow the messages to
accept the generated baseline after validating it."

  (dolist (lang-file lang-files)
    (with-temp-buffer

      (let ((start-time (current-time)))
        (message "START: %s %s" test-name lang-file)

        (t-utils--insert-file-for-test lang-file)

        (let* ((got "")
               (expected-file (replace-regexp-in-string "\\.[^.]+$" "_expected.txt" lang-file))
               (got-file (concat expected-file "~"))
               (expected (when (file-exists-p expected-file)
                           (with-temp-buffer
                             (insert-file-contents-literally expected-file)
                             (buffer-string)))))
          (forward-line) ;; skip the mode line specification
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

          (kill-buffer)
          (when (not (string= got expected))
            (let ((coding-system-for-write 'raw-text-unix))
              (write-region got nil got-file))
            (when (not expected)
              (error "Baseline for %s does not exists.  \
See %s and if it looks good rename it to %s"
                     lang-file got-file expected-file))
            (error "Baseline for %s does not match, got: %s, expected: %s"
                   lang-file got-file expected-file)))
        (message "PASS: %s %s %s" test-name lang-file (t-utils--took start-time))))))

(defun t-utils-test-treesit-defun-name (test-name lang-files)
  "Test `treesit-defun-name-function' setup.
Compare the result of `treesit-defun-name-function' against each
tree-sitter node in each NAME.EXT of LANG-FILES against
NAME_expected.txt.  TEST-NAME is used in messages.

If NAME_expected.txt does not exist or the result doesn't match
NAME_expected.txt, NAME_expected.txt~ will be created.  You are then
instructured to validate the result and rename NAME_expected.txt~
to NAME_expected.txt.

To add a test for TEST-NAME.el which call this function, in the
corresponding TEST-NAME-files/ directory, create
TEST-NAME-files/NAME.EXT, then run the test.  Follow the messages to
accept the generated baseline after validating it."

  (dolist (lang-file lang-files)
    (with-temp-buffer

      (let ((start-time (current-time)))
        (message "START: %s %s" test-name lang-file)

        (t-utils--insert-file-for-test lang-file)

        (let* ((root (treesit-buffer-root-node))
               (expected-file (replace-regexp-in-string "\\.[^.]+$" "_expected.txt" lang-file))
               (expected (when (file-exists-p expected-file)
                           (with-temp-buffer
                             (insert-file-contents-literally expected-file)
                             (buffer-string))))
               (got "")
               (got-file (concat expected-file "~")))

          (treesit-search-subtree
           root
           (lambda (node)
             (let ((defun-name (funcall treesit-defun-name-function node))
                   (node-type (replace-regexp-in-string "\n" "\\n" (treesit-node-type node)))
                   (node-start (treesit-node-start node))
                   (node-end (treesit-node-end node)))
               (setq got (concat
                          got
                          (format "Node %25s at %4d to %4d: defun-name = %s\n"
                                  node-type node-start node-end (if defun-name defun-name "nil")))))
             nil))

          (kill-buffer)
          (when (not (string= got expected))
            (let ((coding-system-for-write 'raw-text-unix))
              (write-region got nil got-file))
            (when (not expected)
              (error "Baseline for %s does not exists.  \
See %s and if it looks good rename it to %s"
                     lang-file got-file expected-file))
            (error "Baseline for %s does not match, got: %s, expected: %s"
                   lang-file got-file expected-file)))
        (message "PASS: %s %s %s" test-name lang-file (t-utils--took start-time))))))

(defun t-utils-test-imenu (test-name lang-files)
  "Test imenu support.
Compare the result of `imenu-create-index-function' on each NAME.EXT in
LANG-FILES against NAME_expected.txt.  TEST-NAME is used in messages.

If NAME_expected.txt does not exist or the result doesn't match
NAME_expected.txt, NAME_expected.txt~ will be created.  You are then
instructured to validate the result and rename NAME_expected.txt~
to NAME_expected.txt.

To add a test for TEST-NAME.el which call this function, in the
corresponding TEST-NAME-files/ directory, create
TEST-NAME-files/NAME.EXT, then run the test.  Follow the messages to
accept the generated baseline after validating it."

  (dolist (lang-file lang-files)
    (with-temp-buffer

      (let ((start-time (current-time)))
        (message "START: %s %s" test-name lang-file)

        (t-utils--insert-file-for-test lang-file)

        (let* ((index (funcall imenu-create-index-function))
               (expected-file (replace-regexp-in-string "\\.[^.]+$" "_expected.txt" lang-file))
               (expected (when (file-exists-p expected-file)
                           (with-temp-buffer
                             (insert-file-contents-literally expected-file)
                             (buffer-string))))
               (got (concat (string-join
                             (mapcar (lambda (el) (substring-no-properties (car el))) index)
                             "\n")
                            "\n"))
               (got-file (concat expected-file "~")))

          (kill-buffer)
          (when (not (string= got expected))
            (let ((coding-system-for-write 'raw-text-unix))
              (write-region got nil got-file))
            (when (not expected)
              (error "Baseline for %s does not exists.  \
See %s and if it looks good rename it to %s"
                     lang-file got-file expected-file))
            (error "Baseline for %s does not match, got: %s, expected: %s"
                   lang-file got-file expected-file)))
        (message "PASS: %s %s %s" test-name lang-file (t-utils--took start-time))))))

(defun t-utils-test-outline-search-function (test-name lang-files)
  "Test setup for `outline-minor-mode'.
Compare the result of `outline-search-function' on each NAME.EXT in
LANG-FILES against NAME_expected.txt.  TEST-NAME is used in messages.

If NAME_expected.txt does not exist or the result doesn't match
NAME_expected.txt, NAME_expected.txt~ will be created.  You are then
instructured to validate the result and rename NAME_expected.txt~
to NAME_expected.txt.

To add a test for TEST-NAME.el which call this function, in the
corresponding TEST-NAME-files/ directory, create
TEST-NAME-files/NAME.EXT, then run the test.  Follow the messages to
accept the generated baseline after validating it."

  (dolist (lang-file lang-files)
    (with-temp-buffer

      (let ((start-time (current-time))
            (lang-file-base (file-name-nondirectory lang-file)))
        (message "START: %s %s" test-name lang-file)

        (t-utils--insert-file-for-test lang-file)

        (let* ((expected-file (replace-regexp-in-string "\\.[^.]+$" "_expected.txt" lang-file))
               (expected (when (file-exists-p expected-file)
                           (with-temp-buffer
                             (insert-file-contents-literally expected-file)
                             (buffer-string))))
               (got "Section heading lines\n\n")
               (got-file (concat expected-file "~")))

          (while (not (eobp))
            (let ((next-heading (funcall outline-search-function)))
              (if next-heading
                  (let ((heading-info (format "%s:%d: %s\n"
                                              lang-file-base
                                              (line-number-at-pos)
                                              (buffer-substring-no-properties
                                               (line-beginning-position) (line-end-position)))))
                    (setq got (concat got heading-info))
                    (forward-line))
                (goto-char (point-max)))))

          (kill-buffer)
          (when (not (string= got expected))
            (let ((coding-system-for-write 'raw-text-unix))
              (write-region got nil got-file))
            (when (not expected)
              (error "Baseline for %s does not exists.  \
See %s and if it looks good rename it to %s"
                     lang-file got-file expected-file))
            (error "Baseline for %s does not match, got: %s, expected: %s"
                   lang-file got-file expected-file)))
        (message "PASS: %s %s %s" test-name lang-file (t-utils--took start-time))))))

(provide 't-utils)
;;; t-utils.el ends here
