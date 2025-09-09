;;; test-matlab-is-matlab-file.el --- -*- lexical-binding: t -*-
;;
;; Copyright 2025 Free Software Foundation, Inc.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;

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
         m-file-major-mode
         is-matlab-file)
    (when zip-buf
      (kill-buffer zip-buf))
    (setq zip-buf (find-file-noselect m-file))

    (with-current-buffer zip-buf
      (goto-char (point-min))
      (re-search-forward "\\.m$")
      (let ((m-buf (archive-extract)))
        (setq m-file-major-mode major-mode)
        (setq is-matlab-file (matlab-is-matlab-file))
        (kill-buffer m-buf)))

    (kill-buffer zip-buf)
    (should (or (eq m-file-major-mode 'matlab-ts-mode)
                (eq m-file-major-mode 'matlab-mode)))
    (should (eq is-matlab-file t))))

(provide 'test-matlab-is-matlab-file)
;;; test-matlab-is-matlab-file.el ends here

;; LocalWords:  buf setq noselect
