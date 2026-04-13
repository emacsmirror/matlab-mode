;;; test-matlab-ts-mode--ei-classify-matrix.el --- -*- lexical-binding: t -*-


;;; Commentary:
;;   Exercise matlab-ts-mode--ei-classify-matrix by calling
;;   matlab-ts-mode--ei-line-nodes-in-region and then
;;   inspect 'm-matrix-info text properties on newlines.
;;
;;   Test cases are
;;      ./test-matlab-ts-mode--ei-classify-matrix-files/NAME.m
;;   which are compared against
;;      ./test-matlab-ts-mode--ei-classify-matrix-files/NAME_expected.m

;;; Code:

(require 't-utils)
(require 'matlab-ts-mode--ei)

(defvar test-matlab-ts-mode--ei-classify-matrix--file nil)

(defun test-matlab-ts-mode--ei-classify-matrix--file (m-file)
  "Test an individual M-FILE.
This is provided for debugging.
  M-: (test-matlab-ts-mode--ei-classify-matrix--file
      \"test-matlab-ts-mode--ei-classify-matrix-files/M-FILE\")"
  (let ((test-matlab-ts-mode--ei-classify-matrix--file m-file))
    (ert-run-tests-interactively "^test-matlab-ts-mode--ei-classify-matrix$")))

(defun test-matlab-ts-mode--ei-classify-matrix-action-fun ()
  "Exercise `matlab-ts-mode--ei-classify-matrix' on the current buffer.
This is done by running `matlab-ts-mode--ei-line-nodes-in-region'
on the test m-file in the current temporary buffer and then
returning the \\='m-matrix-info text properties on newlines along
with text from the m-file as a string."
  (let ((result ""))

    ;; Compute text properties
    (matlab-ts-mode--ei-line-nodes-in-region (point-min) (point-max))

    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((eol-pt (pos-eol))
               (m-matrix-info (get-text-property eol-pt 'm-matrix-info)))
          (when m-matrix-info
            (let ((line-text (buffer-substring (pos-bol) eol-pt)))
              (setq result (concat result (format "L%-3d %-40s | %s\n"
                                                  (line-number-at-pos)
                                                  (format "%S" m-matrix-info)
                                                  line-text)))
              )))
        (forward-line)))
    result))

(ert-deftest test-matlab-ts-mode--ei-classify-matrix ()
  "Test `matlab-ts-mode--ei-classify-matrix'.
Using ./test-matlab-ts-mode--ei-classify-matrix-files/NAME.m, compare
`matlab-ts-mode--ei-classify-matrix' against
./test-matlab-ts-mode--ei-classify-matrix-files/NAME_expected.txt.  This loops
on all ./test-matlab-ts-mode--ei-classify-matrix-files/NAME.m files.

To add a test, create
  ./test-matlab-ts-mode--ei-classify-matrix-files/NAME.m
and run this function.  The baseline is saved for you as
  ./test-matlab-ts-mode--ei-classify-matrix-files/NAME_expected.txt~
after validating it, rename it to
  ./test-matlab-ts-mode--ei-classify-matrix-files/NAME_expected.txt"

  (let* ((test-name "test-matlab-ts-mode--ei-classify-matrix")
         (m-files (t-utils-get-files test-name
                                     :base-regexp (rx ".m" eos)
                                     :file-to-use test-matlab-ts-mode--ei-classify-matrix--file)))
    (t-utils-error-if-no-treesit-for 'matlab test-name)
    (t-utils-test-action test-name m-files
                         :action-fun #'test-matlab-ts-mode--ei-classify-matrix-action-fun)))

(provide 'test-matlab-ts-mode--ei-classify-matrix)
;;; test-matlab-ts-mode--ei-classify-matrix.el ends here

;; LocalWords:  bol defun setq dolist gethash cdr eol prin eos treesit
