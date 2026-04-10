;;; test-matlab-ts-mode--ei-workaround-143.el --- -*- lexical-binding: t -*-

;;; Commentary:
;;   Unit test for `matlab-ts-mode--ei-workaround-143'.
;;

;;; Code:

(require 't-utils)
(require 'matlab-ts-mode--ei)

(defvar test-matlab-ts-mode--ei-workaround-143--file nil)

(defun test-matlab-ts-mode--ei-workaround-143--file (m-file)
  "Test an individual M-FILE.
This is provided for debugging.
  M-: (test-matlab-ts-mode--ei-workaround-143--file
      \"test-matlab-ts-mode--ei-workaround-143-files/M-FILE\")"
  (let ((test-matlab-ts-mode--ei-workaround-143--file m-file))
    (ert-run-tests-interactively "^test-matlab-ts-mode--ei-workaround-143$")))

(defun test-matlab-ts-mode--ei-workaround-143-action-fun ()
  "Exercise `matlab-ts-mode--ei-workaround-143'.
Calls
  (matlab-ts-mode--ei-workaround-143 BEG END)

BEG, END regions are defined by comments:

   % BEG-point                // at start of line

   % END-point                // at end of prior line

If BEG-point comment is not present, point-min is used.  If END-point
comment is not present point-max is used.

A buffer point comment must be present to specify the buffer point:

   % Buffer-point-next-line-column: COL-IDX  // 0 based

Returns the new buffer contents with the updated point marked with <!PT!>."
  (let (beg
        end)

    (goto-char (point-min))

    (if (re-search-forward "% BEG-point" nil t)
        (setq beg (pos-bol))
      (setq beg (point-min)))

    (if (re-search-forward "% END-point" nil t)
        (setq end (1- (pos-bol)))
      (setq end (point-max)))

    (goto-char (point-min))
    (re-search-forward "Buffer-point-next-line-column: *\\([0-9]+\\)")
    (let ((col (string-to-number (match-string 1))))
      (forward-line)
      (move-to-column col)
      (when (not (= (current-column) col))
        (error "Buffer-point-next-line-column is not valid")))

    (setq end (matlab-ts-mode--ei-workaround-143 beg end))

    (save-excursion
      (goto-char end)
      (cl-assert (looking-at (rx eol))))
    
    (let* ((new-pt (point))
           (before-pt (buffer-substring-no-properties (point-min) new-pt))
           (after-pt (buffer-substring-no-properties new-pt (point-max))))
      ;; result
      (concat before-pt "<!PT!>" after-pt))))

(ert-deftest test-matlab-ts-mode--ei-workaround-143 ()
  "Test `matlab-ts-mode--ei-workaround-143'.
Load ./test-matlab-ts-mode--ei-workaround-143-files/NAME.m in to a
temporary buffer, run
`test-matlab-ts-mode--ei-workaround-143-action-fun' that calls
`matlab-ts-mode--ei-workaround-143' and compare results against
./test-matlab-ts-mode--ei-workaround-143-files/NAME_expected.txt.  This
loops on all ./test-matlab-ts-mode--ei-workaround-143-files/NAME.m
files.

To add a test, create
  ./test-matlab-ts-mode--ei-workaround-143-files/NAME.m
and run this function.  The baseline is saved for you as
  ./test-matlab-ts-mode--ei-workaround-143-files/NAME_expected.txt~
after validating it, rename it to
  ./test-matlab-ts-mode--ei-workaround-143-files/NAME_expected.txt"

  (let* ((test-name "test-matlab-ts-mode--ei-workaround-143")
         (m-files (t-utils-get-files test-name
                                     :base-regexp (rx ".m" eos)
                                     :file-to-use test-matlab-ts-mode--ei-workaround-143--file)))
    (t-utils-error-if-no-treesit-for 'matlab test-name)
    (t-utils-test-action test-name m-files
                         :action-fun #'test-matlab-ts-mode--ei-workaround-143-action-fun)))

(provide 'test-matlab-ts-mode--ei-workaround-143)
;;; test-matlab-ts-mode--ei-workaround-143.el ends here

;; LocalWords:  bol defun setq dolist gethash cdr eol prin eos treesit
