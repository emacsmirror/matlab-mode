;;; flycheck-matlab-mlint.el --- Flycheck: MATLAB support -*- lexical-binding: t -*-

;;; Commentary:

;; Provides support for MATLAB code checking with mlint and flycheck
;; that integrates with matlab-mode
;;
;; Usage:
;;
;;     (eval-after-load 'flycheck
;;       '(add-hook 'flycheck-mode-hook #'flycheck-matlab-mlint-setup))

;;; Code:

(defvar flycheck-checkers) ;; incase flycheck is not on the path

(eval-and-compile
  (when (not (require 'flycheck nil 'noerror))
    (defmacro flycheck-define-checker (symbol _docstring &rest _properties)
      "To use flycheck SYMBOL, they need to install flycheck."
      (message "To use %S flycheck, you need to install flycheck." symbol))))

(flycheck-define-checker matlab-mlint
  "A MATLAB checker using MATLAB mlint code analyzer."
  ;; xxx command
  :command ("mlint" "-id" "-all" source-original)
  ;; Example mlint messages.
  ;; L 588 (C 46-49): LOAD: To avoid conflicts with functions on the path, specify variables to load from file.
  ((warning line-start "L " line " (C " column "-" column "): " (id (* alnum)) ":" (message))
   (warning line-start "L " line " (C " column "): " (id (* alnum)) ":" (message)))
  :modes (matlab-mode)
  :predicate (lambda () (flycheck-buffer-saved-p)))

;;;###autoload
(defun flycheck-matlab-mlint-setup ()
  "Set up Flycheck MATLAB mlint.

Adds `matlab-mlint' to `flycheck-checkers'."
  (interactive)
  (add-to-list 'flycheck-checkers 'matlab-mlint))


(provide 'flycheck-matlab-mlint)
;;; flycheck-matlab-mlint.el ends here
