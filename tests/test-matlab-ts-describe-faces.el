;;; test-matlab-ts-describe-faces.el --- -*- lexical-binding: t -*-

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

;;; Code:

(require 'subr-x)

(require 't-utils)

(ert-deftest test-matlab-ts-describe-faces ()
  "Verify `matlab-ts-describe-face' has all faces used by matlab-ts-mode.el.
The faces used in maltab-ts-mode.el and matlab-sections.el must be described in
`matlab-ts-describe-face'."

  (let* ((me (symbol-file 'test-matlab-ts-describe-faces))
         (my-dir (file-name-directory me))
         (proj-root-dir (file-name-directory (directory-file-name my-dir)))
         (matlab-ts-mode-el (concat proj-root-dir "matlab-ts-mode.el"))
         (matlab-sections-el (concat proj-root-dir "matlab-sections.el"))
         (errors '()))
    (with-temp-buffer
      (insert-file-contents-literally matlab-ts-mode-el)
      (insert-file-contents-literally matlab-sections-el)

      (let ((face-re "\\([-a-z]+[a-z]-face\\)")
            (described-faces-ht (make-hash-table :test 'equal))
            (used-faces-ht (make-hash-table :test 'equal)))

        ;; Populate described-faces-ht with string key FACE and value t for all faces mentioned in
        ;;    (defun matlab-ts-describe-faces ()
        ;;     ...
        ;;    )
        ;; then delete the function from the temp buffer
        (goto-char (point-min))
        (re-search-forward "^(defun matlab-ts-describe-faces ()")
        (beginning-of-line)
        (let* ((start-pt (point))
               (end-pt (save-excursion
                         (forward-sexp)
                         (point))))
          (while (re-search-forward face-re end-pt t)
            (let ((face (match-string 1)))
              (puthash face t described-faces-ht)))
          (delete-region start-pt end-pt))

        ;; Eliminate all the (defface ...) statements because these can reference other faces
        (goto-char (point-min))
        (while (re-search-forward "^(defface " nil t)
          (beginning-of-line)
          (let* ((start-pt (point))
                 (end-pt (save-excursion
                           (forward-sexp)
                           (point))))
            (delete-region start-pt end-pt)))
        
        ;; Populate used-faces-ht with string key FACE and value t for all faces mentioned elsewhere
        ;; in matlab-ts-mode.el and matlab-sections.el (the content of the current temp buffer)
        (goto-char (point-min))
        (while (re-search-forward face-re nil t)
          (let ((face (match-string 1)))
            (when (not (string= face "customize-face"))
              (puthash face t used-faces-ht))))

        (let ((errors '()))
          
          (dolist (face (hash-table-keys used-faces-ht))
            (when (not (gethash face described-faces-ht))
              (push (concat face " is not described in matlab-ts-describe-faces")
                    errors)))

          (dolist (described-face (hash-table-keys described-faces-ht))
            (when (not (gethash described-face used-faces-ht))
              (push (concat described-face " is described in matlab-ts-describe-faces but not used")
                    errors)))
        
          )))
    (should (eq errors '()))))


(provide 'test-matlab-ts-describe-faces)
;;; test-matlab-ts-describe-faces.el ends here
