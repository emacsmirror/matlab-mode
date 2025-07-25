;;; eval-buffer-fully.el --- -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Development utiltity useful after modifying a Lisp buffer
;;
;;    M-x eval-buffer-fully

;;; Code:

(defun eval-buffer-fully ()
  "Fully evaluate the buffer.
Evaluate a Lisp buffer by running \\[eval-buffer].  If the buffer has
already been evaluated, `eval-buffer' will only re-evaluate `defun'
defintions.  Therefore, this function will search for all `defvar',
`defvar-local', and `defface' defintions on their own line and run
\\[eval-defun] on them."
  (interactive)
  (save-excursion
    (eval-buffer)
    (goto-char (point-min))
    (while (re-search-forward
            "^[ \t]*([ \t\r\n]*\\(?:defvar\\|defvar-local\\|defface\\)[ \t\r\n]+[^ \t\r\n]+"
            nil t)
      (eval-defun nil))))

(provide 'eval-buffer-fully)
;;; eval-buffer-fully.el ends here
