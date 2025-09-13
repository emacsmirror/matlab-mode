;;; test-matlab-is-matlab-file.el --- -*- lexical-binding: t -*-
;;
;; Copyright 2025 Free Software Foundation, Inc.
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
extract the first *.m file in it and validate it enters a MATLAB mode.
Also, validate `matlab-is-matlab-file' returns t."
  (let* ((m-file "test-matlab-is-matlab-file-files/archive.zip")
         (zip-buf (get-file-buffer m-file))
         (all-entered-a-matlab-mode 'unknown)
         (all-are-matlab-file 'unknown))
    (when zip-buf
      (kill-buffer zip-buf))
    (setq zip-buf (find-file-noselect m-file))

    (with-current-buffer zip-buf
      (goto-char (point-min))
      (while (re-search-forward "\\.m$" nil t)
        (let ((m-buf (archive-extract))
              (is-a-matlab-mode (or (eq major-mode 'matlab-ts-mode)
                                    (eq major-mode 'matlab-mode)))
              (is-matlab-file (matlab-is-matlab-file)))
          (message "test-matlab-is-matlab-file: checking %s" (buffer-file-name))
          (when (or (eq all-entered-a-matlab-mode 'unknown)
                    (not is-a-matlab-mode))
            (setq all-entered-a-matlab-mode is-a-matlab-mode))

          (when (or (eq all-are-matlab-file 'unknown)
                    (not is-matlab-file))
            (setq all-are-matlab-file is-matlab-file))
          (kill-buffer m-buf)
          (set-buffer zip-buf))))

    (kill-buffer zip-buf)
    (should (eq all-entered-a-matlab-mode t))
    (should (eq all-are-matlab-file t))))

(provide 'test-matlab-is-matlab-file)
;;; test-matlab-is-matlab-file.el ends here

;; LocalWords:  buf setq noselect
