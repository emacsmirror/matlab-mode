;;; test-matlab-shell-scan-for-error.el --- -*- lexical-binding: t -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.
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
;; Test `matlab-shell-scan-for-error'

;;; Code:

(require 't-utils)
(require 'matlab-shell)

(ert-deftest test-matlab-shell-scan-for-error ()
  "Test `matlab-shell-scan-for-error'.
Each ./test-matlab-shell-scan-for-error-files/NAME.org representing
MATLAB shell output is scanned for errors and the results are compared
against ./test-matlab-shell-scan-for-error-files/NAME_expected.txt.

The format of NAME.org is
  * Error patter description

  #+begin_example
  <Output in matlab-shell starting at column 0>
  #=end_example

To add a test, create
  ./test-matlab-shell-scan-for-error-files/NAME.org
and run this function.  The baseline is saved for you as
  ./test-matlab-shell-scan-for-error-files/NAME_expected.txt~
after validating them, rename them to
  ./test-matlab-shell-scan-for-error-files/NAME_expected.txt"

  (let* ((test-name "test-matlab-shell-scan-for-error")
         (org-files (t-utils-get-files test-name
                                       :base-regexp (rx ".org" eos)))
         (error-msgs '()))
    (dolist (org-file org-files)
      (let ((start-time (current-time))
            (got-matches '()) ;; Entries of form (linenum . ans-from--matlab-shell-scan-for-error)
            (expected-file (replace-regexp-in-string "\\.[^.]+\\'" "_expected.txt" org-file))
            limit)
        (with-temp-buffer
          (insert-file-contents-literally org-file)
          (goto-char (point-max))
          (re-search-backward "#\\+end_example")
          (while (setq limit (save-excursion (when (re-search-backward "#\\+begin_example" nil t)
                                               (point))))
            (let ((ans (matlab-shell-scan-for-error limit)))
              (when (not ans)
                (error "%s:%d: error: failed to find error pattern in example block"
                       org-file (line-number-at-pos limit)))
              (push `(,(line-number-at-pos (car ans)) . ,ans) got-matches))
            (goto-char limit)))

        (let ((got (let ((org-file-base (file-name-nondirectory org-file))
                         (s ""))
                     (dolist (got-match got-matches)
                       (setq s (concat s (format "%s:%d: %S\n"
                                                 org-file-base (car got-match) (cdr got-match)))))
                     s)))
          (let ((error-msg (t-utils--baseline-check test-name start-time
                                                    org-file got expected-file)))
            (when error-msg
              (push error-msg error-msgs))))))

    (setq error-msgs (reverse error-msgs))
    (should (equal error-msgs '()))))

(provide 'test-matlab-shell-scan-for-error)
;;; test-matlab-shell-scan-for-error.el ends here

;; LocalWords:  utils dolist setq
