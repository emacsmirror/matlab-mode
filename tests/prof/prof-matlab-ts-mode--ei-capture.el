;;; prof-matlab-ts-mode--ei-capture.el --- -*- lexical-binding: t -*-

;; Version: 8.0.0
;; URL: https://github.com/mathworks/Emacs-MATLAB-Mode
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Author: John Ciolfi <john.ciolfi.32@gmail.com>
;; Keywords: MATLAB

;; Copyright (C) 2026 Free Software Foundation, Inc.
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
;;    Manually profile tree-sitter node captures.


;;; Code:

(require 'profiler)
(require 'matlab-ts-mode)

(defun matlab-ts-mode--ei-elapsed-time (start-time)
  "Return elapsed time string, now - START-TIME."
  (concat "Elapsed time: "
          (string-trim (format "%10.1f" (float-time (time-subtract (current-time) start-time))))
          " seconds."))

(defvar matlab-ts-mode--ei-all-nodes-query (treesit-query-compile 'matlab '(_ @n)))

(defun matlab-ts-mode--ei-nodes-in-region (beg end)
  "Get leave nodes in region BEG to END."
  (matlab-ts-mode--ei-fast-back-to-indentation)
  (let ((region-nodes (treesit-query-capture (treesit-buffer-root-node 'matlab)
                                             matlab-ts-mode--ei-all-nodes-query beg end t))
        leaf-nodes)
    (dolist (node region-nodes)
      (when (= (treesit-node-child-count node) 0)
        (push node leaf-nodes)))
    (reverse leaf-nodes)))

(defun prof-matlab-ts-mode--ei-nodes-region (arg)
  "Profile `matlab-ts-mode--ei-nodes-in-region'.
This profiles the current `matlab-ts-mode' buffer where region is the
whole buffer.  With prefix ARG, report elapsed time without profiling."
  (interactive "P")
  (when (not (eq major-mode 'matlab-ts-mode))
    (user-error "Buffer %s major-mode is not matlab-ts-mode" (buffer-name)))
  (goto-char (point-min))
  (let ((start-time (current-time))
        leaf-nodes)
    (when (not arg)
      (profiler-start 'cpu))
    (unwind-protect
        (setq leaf-nodes (matlab-ts-mode--ei-nodes-in-region (point-min) (point-max)))
      (when (not arg)
        (profiler-stop)
        (profiler-report)))
    (message "Found %d leaf nodes. %s" (length leaf-nodes)
             (matlab-ts-mode--ei-elapsed-time start-time))))

(defun matlab-ts-mode--ei-nodes-in-line ()
  "Get leave nodes in current line."
  (matlab-ts-mode--ei-fast-back-to-indentation)
  (let ((line-nodes (treesit-query-capture (treesit-buffer-root-node)
                                           matlab-ts-mode--ei-all-nodes-query (point) (pos-eol) t))
        line-leaf-nodes)
    (dolist (node line-nodes)
      (when (= (treesit-node-child-count node) 0)
        (push node line-leaf-nodes)))
    (reverse line-leaf-nodes)))

(defun prof-matlab-ts-mode--ei-nodes-in-line (arg)
  "Profile `matlab-ts-mode--ei-nodes-in-line'.
This profiles the current `matlab-ts-mode' buffer.
With prefix ARG, report elapsed time without profiling."
  (interactive "P")
  (when (not (eq major-mode 'matlab-ts-mode))
    (user-error "Buffer %s major-mode is not matlab-ts-mode" (buffer-name)))
  (goto-char (point-min))
  (let ((start-time (current-time))
        (count 0))
    (when (not arg)
      (profiler-start 'cpu))
    (unwind-protect
        (while (not (eobp))
           (let ((line-leaf-nodes (matlab-ts-mode--ei-nodes-in-line)))
             (setq count (+ count (length line-leaf-nodes))))
           (forward-line))
      (when (not arg)
        (profiler-stop)
        (profiler-report)))
    (message "Found %d leaf nodes. %s" count
             (matlab-ts-mode--ei-elapsed-time start-time))))

(provide 'prof-matlab-ts-mode--ei-capture)
;;; prof-matlab-ts-mode--ei-capture.el ends here
