;;; test-matlab--ei-offset.el --- -*- lexical-binding: t -*-


;;; Commentary:
;;   Unit test for `matlab--ei-offset' and `matlab--ei-move-to-offset'
;;

;;; Code:

(require 't-utils)
(require 'matlab-ts-mode--ei)

(defvar test-matlab--ei-offset--file nil)

(defun test-matlab--ei-offset--file (txt-file)
  "Test an individual TXT-FILE.
This is provided for debugging.
  M-: (test-matlab--ei-offset--file
      \"test-matlab--ei-offset-files/TXT-FILE\")"
  (let ((test-matlab--ei-offset--file txt-file))
    (ert-run-tests-interactively "^test-matlab--ei-offset$")))

(defun test-matlab--ei-offset-action-fun ()
  "Exercise `matlab--ei-offset'.
In buffer search for ampersand character and use
that to exercise `matlab--ei-offset' and
`matlab--ei-move-to-offset'

Returns string with the offset locations:

Line content                                                Offset
----------------------------------------------------------  ------
<line-content>                                              <index>"

  (let ((result (concat "Line content                                                Offset\n"
                        "----------------------------------------------------------  ------\n")))

    (goto-char (point-min))

    (while (re-search-forward "&" nil t)
      (backward-char)
      (let* ((pt (point))
             (offset (matlab--ei-offset))
             (offset-via-pt (matlab--ei-offset pt)))
        (cl-assert (= offset offset-via-pt))

        (end-of-line)
        (cl-assert (= pt (progn (matlab--ei-move-to-offset offset) (point))))
        
        (setq result (concat result
                             (format "%-58s  %6d\n"
                                     (buffer-substring (pos-bol) (pos-eol))
                                     offset))))
      (forward-char))

    result))

(ert-deftest test-matlab--ei-offset ()
  "Test `matlab--ei-offset' and `matlab--ei-move-to-offset'.
Using ./test-matlab--ei-offset-files/NAME.txt, run `matlab--ei-offset'
and `matlab--ei-move-to-offset' leveraging ampersand characters for the
points to examine.  Compare result from
`test-matlab--ei-offset-action-fun' against
./test-matlab--ei-offset-files/NAME_expected.txt.  This loops on all
./test-matlab--ei-offset-files/NAME.txt files.

To add a test, create
  ./test-matlab--ei-offset-files/NAME.txt
and run this function.  The baseline is saved for you as
  ./test-matlab--ei-offset-files/NAME_expected.txt~
after validating it, rename it to
  ./test-matlab--ei-offset-files/NAME_expected.txt"

  (let* ((test-name "test-matlab--ei-offset")
         (txt-files (t-utils-get-files test-name
                                       :base-regexp (rx ".txt" eos)
                                       :skip-regexp (rx "_expected.txt" eos)
                                       :file-to-use test-matlab--ei-offset--file)))
    (t-utils-error-if-no-treesit-for 'matlab test-name)
    (t-utils-test-action test-name txt-files
                         :action-fun #'test-matlab--ei-offset-action-fun)))

(provide 'test-matlab--ei-offset)
;;; test-matlab--ei-offset.el ends here

;; LocalWords:  bol defun setq dolist gethash cdr eol prin eos treesit
