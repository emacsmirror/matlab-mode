;;; sweep-test-matlab-ts-grammar.el --- -*- lexical-binding: t -*-
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
;; M-: (sweep-test-matlab-ts-grammar)  - Look for bad matlab tree-sitter parses
;;                                       on *.m files in current directory
;;


;;; Code:

(require 't-utils)
(require 'matlab-ts-mode)
(require 'matlab--access)


(defun sweep-test-matlab-ts-grammar--syntax-checker (m-files)
  "Syntax check each *.m file in M-FILES using MATLAB checkIssue.

Returns hash table where the keys are the m-files and each key
value is either \"no-syntax-errors\" or \"has-syntax-errors\"."
  (let* ((matlab-exe (or (matlab--get-abs-matlab-exe)
                         (error "No matlab found (to fix put matlab on your PATH)")))
         (tmp-check-file (make-temp-file "sweep_test_matlab_ts_grammar" nil ".m"))
         (check-fun (file-name-sans-extension (file-name-nondirectory tmp-check-file)))
         (tmp-check-file-dir (file-name-directory tmp-check-file))
         (result-ht (make-hash-table :test 'equal)))

    (with-temp-buffer
      (cd tmp-check-file-dir)
      (insert "filesToCheck = ...
    [
")

      (dolist (m-file m-files)
        (insert "      \"" m-file "\"\n"))

      (insert "    ];

for fIdx = 1:length(filesToCheck)
    file = filesToCheck(fIdx);
    issues = codeIssues(file);

    errorIdx = [];
    for issueIdx = 1 : size(issues.Issues, 1)
        if ~isequal(issues.Issues.Severity(issueIdx), matlab.codeanalysis.IssueSeverity.error)
            continue;
        end

        % Ignore mismatch in class/function name because it is not an error when executed.
        % \"foo.m\" error auto  \"Class name 'foo1' and file name do not agree ....\"
        desc = issues.Issues.Description(issueIdx);
        if regexp(desc, \"^Class name .* and file name do not agree\", 'once')
            continue
        end
        errorIdx = issueIdx;
        break
    end

    if ~isempty(errorIdx)
        syntaxStatus = sprintf(\"has-syntax-errors at line %d:%d to %d:%d - %s\", ...
                               issues.Issues.LineStart(errorIdx), ...
                               issues.Issues.ColumnStart(errorIdx), ...
                               issues.Issues.LineEnd(errorIdx), ...
                               issues.Issues.ColumnEnd(errorIdx), ...
                               issues.Issues.Description(errorIdx));
    else
        syntaxStatus = \"no-syntax-errors\";
    end
    disp(strcat(\"--> \", file, \" > \", syntaxStatus));
end
");
      (let ((coding-system-for-write 'raw-text-unix))
        (write-region (point-min) (point-max) tmp-check-file)))

    ;; Run codeIssues(mFile) via: matlab --batch check-fun
    (with-temp-buffer
      (cd tmp-check-file-dir)
      (let ((status (call-process matlab-exe nil t nil "-batch" check-fun)))
        (when (not (= status 0))
          (error "%s -batch %s (in directory %s) returned non-zero status, %d, with output:\n%s"
                 matlab-exe check-fun tmp-check-file-dir status (buffer-string))))
      (goto-char (point-min))

      (while (not (eobp))
        (when (looking-at "^--> \\([^>]+\\) > \\(.+\\)$")
          (let* ((info-line (match-string 0))
                 (m-file (match-string 1))
                 (syntax-status (match-string 2))
                 (syntax-status-pair
                  (cond
                   ((string= syntax-status "no-syntax-errors")
                    (cons syntax-status nil))
                   ((string-match "\\`\\(has-syntax-errors\\) \\(at line [0-9]+:[0-9]+.+\\)\\'"
                                  syntax-status)
                    (cons (match-string 1 syntax-status) (match-string 2 syntax-status)))
                   (t
                    (error "Unexpected result: %s" info-line)))))
          (puthash m-file syntax-status-pair result-ht)))
        (forward-line))

      ;; Validate we got expected stdout:
      ;;    --> M-FILE1 > SYNTAX-STATUS1
      ;;    --> M-FILE2 > SYNTAX-STATUS2
      ;;    ....
      (dolist (m-file m-files)
        (when (not (gethash m-file result-ht))
          (error "%s -batch %s (in directory %s) didn't return expected stdout, got:\n%s"
                 matlab-exe check-fun tmp-check-file-dir (buffer-string)))))

    (delete-file tmp-check-file)
    result-ht))

(defun sweep-test-matlab-ts-grammar (&optional directory log-file)
  "Check matlab tree-sitter parse of all *.m files under DIRECTORY.
DIRECTORY defaults to the current directory.

  \\[sweep-test-matlab-ts-grammar]

This validates that if MATLAB tree-sitter parse has ERROR nodes that the
MATLAB codeIssues command,
https://www.mathworks.com/help/matlab/ref/codeissues.html says the file
has syntax issues (issue servity of error).  Likewise if MATLAB
tree-sitter parse says no syntax errors this test confirms that the
MATLAB codeIssues command reports the same.

Messages are logged to LOG-FILE, which defaults to
sweep-test-matlab-ts-grammar.log

When run interactively, displays the result in a *sweep-test-matlab-ts-grammar*
buffer, otherwise the results are displayed on stdout.

On large directory trees, run via
  cd /path/to/your/directory
  Emacs --batch \\
        -q \\
        -L /path/to/Emacs-MATLAB-Mode \\
        -l /path/to/Emacs-MATLAB-Mode/matlab-autoload.el \\
        -L /path/to/Emacs-MATLAB-Mode/tests/ \\
        -l /path/to/Emacs-MATLAB-Mode/tests/t-utils.el \\
        -l /path/to/Emacs-MATLAB-Mode/tests/sweep-test-matlab-ts-grammar \\
        -f sweep-test-matlab-ts-grammar
to see the progress messages in your terminal.
See ./sweep-test-matlab-ts-grammar.sh"
  (interactive)
  (let ((test-name "sweep-test-matlab-ts-grammar"))
    (t-utils-error-if-no-treesit-for 'matlab test-name)
    (t-utils-sweep-test-ts-grammar test-name
                                   (or directory default-directory)
                                   (rx ".m" eos)
                                   #'matlab-ts-mode
                                   #'sweep-test-matlab-ts-grammar--syntax-checker
                                   nil
                                   log-file)))

(provide 'sweep-test-matlab-ts-grammar)
;;; sweep-test-matlab-ts-grammar.el ends here
