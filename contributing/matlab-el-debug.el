;;; matlab-el-debug.el --- -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses>.

;;; Commentary:
;; MATLAB Emacs Lisp utilties for debugging.
;;

;;; Code:

(require 'subr-x)
(require 'treesit)

(require 'matlab-ts-mode--ei)

(defun matlab-el-debug-print-bol2loc-map ()
  "Print `matlab-ts-mode--ei-bol2loc-map' for debugging purposes."
  (interactive)
  (let ((contents "-->matlab-ts-mode--ei-bol2loc-map\n")
        (bol-points (hash-table-keys matlab-ts-mode--ei-bol2loc-map)))
    (dolist (bol-pt (sort bol-points))
      (let ((loc (gethash bol-pt matlab-ts-mode--ei-bol2loc-map)))
        (setq contents (concat contents (format "   %-4s: point %5d %S\n"
                                                (format "L%d" (line-number-at-pos bol-pt))
                                                bol-pt
                                                (car loc))))))

    (message "%s" contents)))


(provide 'matlab-el-debug)
;;; matlab-el-debug.el ends here
