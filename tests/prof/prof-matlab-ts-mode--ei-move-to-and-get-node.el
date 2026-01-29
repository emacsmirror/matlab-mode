;;; prof-matlab-ts-mode--ei-move-to-and-get-node.el --- -*- lexical-binding: t -*-

;; Version: 8.0.0
;; URL: https://github.com/mathworks/Emacs-MATLAB-Mode
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Author: John Ciolfi <john.ciolfi.32@gmail.com>
;; Created: Dec-29-2025
;; Keywords: MATLAB

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.
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
;;    Manually profile `matlab-ts-mode--ei-move-to-and-get-node'


;;; Code:

(require 'profiler)
(require 'matlab-ts-mode)

(defun prof-matlab-ts-mode--ei-move-to-and-get-node (arg)
  "Profile `matlab-ts-mode--ei-move-to-and-get-node'.
This profiles the current `matlab-ts-mode' buffer.
With prefix ARG, report elapsed time without profiling."
  (interactive "P")
  (when (not (eq major-mode 'matlab-ts-mode))
    (user-error "Buffer %s major-mode is not matlab-ts-mode" (buffer-name)))
  (goto-char (point-min))

  (unwind-protect
      (let ((start-time (current-time)))
         (when (not arg) (profiler-start 'cpu))
         (while (not (eobp))
           (matlab-ts-mode--ei-fast-back-to-indentation)
          (cl-loop while t do
                   (let* ((pair (matlab-ts-mode--ei-move-to-and-get-node))
                          (node (car pair)))
                     (when (not node)
                       (cl-return))
                     (goto-char (treesit-node-end node))))
          (forward-line))
         (message "elapsed time: %10.1f" (float-time (time-subtract (current-time) start-time))))
    (when (not arg)
      (profiler-stop)
      (profiler-report))))

(provide 'prof-matlab-ts-mode--ei-move-to-and-get-node)
;;; prof-matlab-ts-mode--ei-move-to-and-get-node.el ends here
