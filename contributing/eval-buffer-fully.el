;;; eval-buffer-fully.el --- -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Development utiltity useful after modifying a Lisp buffer
;;
;;    M-x eval-buffer-fully

;;; Code:

(defun eval-buffer-fully ()
  "Try to fully evaluate the buffer.
Evaluate a Lisp buffer by running \\[eval-buffer].  If the buffer has
already been evaluated, `eval-buffer' will only re-evaluate `defun'
defintions.  Therefore, this function will search for other defintions
on their own line and run \\[eval-defun] on them.  This will miss some
items such as use of macros."
  (interactive)
  (save-excursion
    (eval-buffer)
    (goto-char (point-min))
    (while (re-search-forward
            (rx (seq bol
                     (0+ (in " " "\t"))
                     "("
                     (or "defvar"
                         "defvar-local"
                         "defface"
                         "defmacro"
                         "easy-menu-define"
                         "defvar-keymap"
                         "define-derived-mode")
                     (1+ (in " " "\t" "\r" "\n"))
                     (not (in " " "\t" "\r" "\n"))
                     ))
            nil t)
      (eval-defun nil))))

(provide 'eval-buffer-fully)
;;; eval-buffer-fully.el ends here
