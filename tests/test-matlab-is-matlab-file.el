;;; test-matlab-is-matlab-file.el --- -*- lexical-binding: t -*-
;;
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
;; Test `matlab-is-matlab-file'
;;

;;; Code:

(require 'matlab-is-matlab-file)
(require 'arc-mode)

(ert-deftest test-matlab-is-matlab-file ()
  "Test `matlab-is-matlab-file'.
Using ./test-matlab-is-matlab-file-files/archive.zip,
extract the *.m files and validate they enter Objective-C mode
if the file starts with objc, else they should enter MATLAB mode.
When the file starts with objc, the content must be Objective-C
content in archive.zip.

Also, validate `matlab-is-matlab-file' returns t or nil."
  (let* ((m-file "test-matlab-is-matlab-file-files/archive.zip")
         (zip-buf (get-file-buffer m-file))
         (all-good t))
    (when zip-buf
      (kill-buffer zip-buf))
    (setq zip-buf (find-file-noselect m-file))

    (with-current-buffer zip-buf
      (goto-char (point-min))
      (while (re-search-forward "\\.m$" nil t)
        (let ((m-buf (archive-extract))
              (is-matlab-file (matlab-is-matlab-file)))
          (message "test-matlab-is-matlab-file: checking %s" (buffer-file-name))

          (cond

           ((string-match-p "^objc" (buffer-name))
            ;; should be objective-c
            (when (not (eq major-mode 'objc-mode))
              (message "error: %s did not enter objc-mode" (buffer-file-name))
              (setq all-good nil))
            (when is-matlab-file
              (message "error: %s says its a MATLAB file" (buffer-file-name))))

           (t
            ;; should be a MATLAB mode
            (when (not (or (eq major-mode 'matlab-ts-mode)
                           (eq major-mode 'matlab-mode)))
              (message "error: %s did not enter a MATLAB mode" (buffer-file-name))
              (setq all-good nil))
            (when (not is-matlab-file)
              (message "error: %s says its NOT a MATLAB file" (buffer-file-name)))))

          (kill-buffer m-buf)
          (set-buffer zip-buf))))

    (kill-buffer zip-buf)
    (should (eq all-good t))))

(provide 'test-matlab-is-matlab-file)
;;; test-matlab-is-matlab-file.el ends here

;; LocalWords:  buf setq noselect
