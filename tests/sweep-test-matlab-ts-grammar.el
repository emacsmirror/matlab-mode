;;; sweep-test-matlab-ts-grammar.el --- -*- lexical-binding: t -*-

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
;; M-: (sweep-test-matlab-ts-grammar)  - Look for bad matlab tree-sitter parses
;;                                       on *.m files in current directory
;;


;;; Code:

(require 't-utils)
(require 'matlab-ts-mode)
(require 'matlab--access)

(defun sweep-test-matlab-ts-grammar--run-syntax-check (matlab-exe m-files)
  "Using MATLAB-EXE check M-FILES using MATLAB checkIssue.
See `sweep-test-matlab-ts-grammar--syntax-checker' for return."

  (let* ((tmp-check-file (make-temp-file "sweep_test_matlab_ts_grammar" nil ".m"))
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

        % --- Following ignored errors are real, but not for matlab tree-sitter ---

        % Ignore: This condition has no effect because all blocks in this if statement are identical
        if regexp(desc, \"^This condition has no effect\", 'once')
            continue
        end

        % Ignore: Using 'isempty' on a logical expression creates incorrect results
        if regexp(desc, \"^Using 'isempty' on a logical expression creates incorrect\", 'once')
            continue
        end

        % Ignore: Invalid MATLAB file name. MATLAB file names must start with a letter, .....
        if regexp(desc, \"^Invalid MATLAB file name. MATLAB file names must start with \", 'once')
            continue
        end

        % Ignore: TRY/CATCH is unsupported for code generation.
        if regexp(desc, \"^TRY/CATCH is unsupported for code generation\", 'once')
            continue
        end

        % Ignore: 'printopt' has been removed. There is no simple replacement for this.
        if regexp(desc, \"^'.+' has been removed\", 'once')
            continue
        end

        % Ignore: The variable 'hObj' is an uplevel variable
        if regexp(desc, \"^The variable '.+' is an uplevel variable\", 'once')
            continue
        end

        % Ignore: To pass MException properties to the warning function, ....
        if regexp(desc, \"^To pass MException properties to the warning function\", 'once')
            continue
        end

        % Ignore: 'JavaFrame' was undocumented and has been removed.
        if regexp(desc, \"^'.+' was undocumented and has been removed\", 'once')
            continue
        end

        % Ignore: A class definition cannot be inside a private directory.
        if regexp(desc, \"^A class definition cannot be inside a private directory\", 'once')
            continue
        end

        % Ignore: The first input argument to 'assert' must be a condition
        if regexp(desc, \"^The first input argument to 'assert' must be a condition\", 'once')
            continue
        end

        % Ignore: The method set.InitialValue does not refer to a valid property name
        if regexp(desc, \"^The method .+ does not refer to a valid property name\", 'once')
            continue
        end

        % Ignore: Code generation does not support 'str2num'. Use 'str2double' instead.
        if regexp(desc, \"^Code generation does not support\", 'once')
            continue
        end

        % Ignore: Code generation requires a variable to be assigned before subscripting it.
        if regexp(desc, \"^Code generation requires a variable to be assigned \", 'once')
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

(defun sweep-test-matlab-ts-grammar--syntax-checker (m-files)
  "Syntax check each *.m file in M-FILES using MATLAB checkIssue.

Returns hash table where the keys are the m-files and each key
value is either \"no-syntax-errors\" or \"has-syntax-errors\"."
  (let ((matlab-exe (matlab--get-abs-matlab-exe 'no-error)))
    (if matlab-exe
        (sweep-test-matlab-ts-grammar--run-syntax-check matlab-exe m-files)
      (message (concat "Unable to use MATLAB codeIssues() command to compare against matlab "
                       "tree-sitter because matlab is not found.\n"
                       "Examine the *.log file for results.\n"))
      nil)))

(defun sweep-test-matlab-ts-grammar (&optional directory log-file)
  "Check matlab tree-sitter parse of all *.m files under DIRECTORY.
DIRECTORY defaults to the current directory.

  \\[sweep-test-matlab-ts-grammar]

This validates that if MATLAB tree-sitter parse has ERROR nodes that the
MATLAB codeIssues command,
https://www.mathworks.com/help/matlab/ref/codeissues.html says the file
has syntax issues (issue severity of error).  Likewise if MATLAB
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

;; LocalWords:  utils defun tmp nondirectory dolist codeanalysis uplevel eobp puthash gethash
;; LocalWords:  treesit eos
