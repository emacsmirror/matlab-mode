;;; test-matlab-ts-mode--ei-classify-matrix.el --- -*- lexical-binding: t -*-


;;; Commentary:
;;   Exercise matlab-ts-mode--ei-classify-matrix by calling
;;   matlab-ts-mode--ei-line-nodes-in-region and then
;;   inspect matlab-ts-mode--ei-m-matrix-pos-bol-map
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
returning the contents of `matlab-ts-mode--ei-m-matrix-pos-bol-map'
and `matlab-ts-mode--ei-m-matrix-node-map' along with text from
the m-file as a string."
  (let ((result "")
        (pos-bol-keys))
          
    ;; Compute maps
    (matlab-ts-mode--ei-line-nodes-in-region (point-min) (point-max))

    (maphash (lambda (key _value)
               (push key pos-bol-keys))
             matlab-ts-mode--ei-m-matrix-pos-bol-map)
    (setq pos-bol-keys (sort pos-bol-keys #'<))
    
    (dolist (pos-bol-key pos-bol-keys)
      (let* ((entry (gethash pos-bol-key matlab-ts-mode--ei-m-matrix-pos-bol-map))
             (matrix-type (car entry))
             (matrix-node (cdr entry))
             (col-widths (when (and matrix-node (not (eq matrix-node 'empty)))
                           (cdr (gethash matrix-node matlab-ts-mode--ei-m-matrix-node-map))))
             (line-text (buffer-substring pos-bol-key (save-excursion
                                                        (goto-char pos-bol-key)
                                                        (pos-eol)))))
        (setq result
              (concat result
                      (format "L%-3d point %-3d => '%-20s col-widths=%-15s | %s\n"
                              (line-number-at-pos pos-bol-key)
                              pos-bol-key
                              (symbol-name matrix-type)
                              (if col-widths
                                  (prin1-to-string col-widths)
                                "nil")
                              line-text)))))
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

;; LocalWords:  bol defun maphash setq dolist gethash cdr eol prin eos treesit
