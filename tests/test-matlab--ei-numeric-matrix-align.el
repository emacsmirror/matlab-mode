;;; test-matlab--ei-numeric-matrix-align.el --- -*- lexical-binding: t -*-


;;; Commentary:
;;   Unit test that validates multi-row numeric matrices, 'numeric-m-matrix alignment
;;   follow the fast alignment code path.
;;

;;; Code:

(require 't-utils)
(require 'matlab-ts-mode--ei)

(defvar test-matlab--ei-numeric-matrix-align--file nil)

(defun test-matlab--ei-numeric-matrix-align--file (txt-file)
  "Test an individual TXT-FILE.
This is provided for debugging.
  M-: (test-matlab--ei-numeric-matrix-align--file
      \"test-matlab--ei-numeric-matrix-align-files/TXT-FILE\")"
  (let ((test-matlab--ei-numeric-matrix-align--file txt-file))
    (ert-run-tests-interactively "^test-matlab--ei-numeric-matrix-align$")))

(defun test-matlab--ei-align-error (_orig-fun &rest _args)
  "Advice that will error if `matlab-ts-mode--ei-align-line-in-m-matrix' is called."
  (error "Double align occurred, matlab-ts-mode--ei-align-line-in-m-matrix was called"))

(defun test-matlab--ei-numeric-matrix-align-action-fun ()
  "Call indent region on the current buffer.
Validate slower `matlab-ts-mode--ei-align-line-in-m-matrix' is not called.
If it is called, then we are doing double alignment."
  (let ((slow-fun #'matlab-ts-mode--ei-align-line-in-m-matrix))
    (unwind-protect
        (progn
          (advice-add slow-fun :around #'test-matlab--ei-align-error)
          (indent-region (point-min) (point-max)))
      (advice-remove slow-fun #'test-matlab--ei-align-error)))
  ;; result is nil indicating no need to create NAME_expected.txt
  nil)

(ert-deftest test-matlab--ei-numeric-matrix-align ()
  "Test \\='numeric-m-matrix alignment code path.
Using ./test-matlab--ei-numeric-matrix-align-files/NAME.m, validates
multi-row numeric matrices, \\='numeric-m-matrix are aligned in
`matlab-ts-mode--ei-get-new-line' are not \"re-aligned\" using
`matlab-ts-mode--ei-align-line-in-m-matrix'.  This loops on all
./test-matlab--ei-numeric-matrix-align-files/NAME.m files and
validates `matlab-ts-mode--ei-align-line-in-m-matrix' is not caled.

To add a test, create
  ./test-matlab--ei-numeric-matrix-align-files/NAME.m"

  (let* ((test-name "test-matlab--ei-numeric-matrix-align")
         (txt-files (t-utils-get-files test-name
                                       :base-regexp (rx ".m" eos)
                                       :file-to-use test-matlab--ei-numeric-matrix-align--file)))
    (t-utils-error-if-no-treesit-for 'matlab test-name)
    (t-utils-test-action test-name txt-files
                         :action-fun #'test-matlab--ei-numeric-matrix-align-action-fun)))

(provide 'test-matlab--ei-numeric-matrix-align)
;;; test-matlab--ei-numeric-matrix-align.el ends here

;; LocalWords:  bol defun setq dolist gethash cdr eol prin eos treesit
