;;; ts-inspect.el --- -*- lexical-binding: t -*-

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
;; Tree-sitter inspection utilities for developing tree-sitter modes
;;

;;; Code:

(require 'treesit)

(defun ts-inspect-point (&optional position)
  "Display information about node at POSITION.
POSITION defaults to the current `point'.
Returns a string of the form:
  Hierarchy:
    #<treesit-node ROOT in 1-67>
      #<treesit-node PARENT in 37-62>
        #<treesit-node NODE in 61-62>
  Prev-siblings:
    > #<treesit-node PREV-SIBLING2 in 59-60>
      > #<treesit-node PREV-SIBLING1 in 60-61>
        > #<treesit-node NODE in 61-62>"

  (when (not position)
    (setq position (point)))

  (let ((node (treesit-node-at position))
        result)

    ;; Hierarchy
    (let* ((tree `(,(format "%S\n" node)))
           (parent (treesit-node-parent node))
           (prefix "  "))
      (while parent
        (push (format "%S\n" parent) tree)
        (setq parent (treesit-node-parent parent)))

      (setq result (concat "Hierarchy:\n"
                           (mapconcat (lambda (n)
                                        (let ((leaf (concat prefix n)))
                                          (setq prefix (concat "  " prefix))
                                          leaf))
                                      tree ""))))

    ;; Prev-siblings
    (let* ((prev-siblings `(,(format "%S\n" node)))
           (prev-sibling (treesit-node-prev-sibling node))
           (prefix "  "))
      (while prev-sibling
        (push (format "%S\n" prev-sibling) prev-siblings)
        (setq prev-sibling (treesit-node-prev-sibling prev-sibling)))

      (setq result (concat result
                           "Prev-siblings:\n"
                           (mapconcat (lambda (n)
                                        (let ((leaf (concat prefix "> " n)))
                                          (setq prefix (concat "  " prefix))
                                          leaf))
                                      prev-siblings ""))))
    result
    ))
  


(provide 'ts-inspect)
;;; ts-inspect.el ends here

;; LocalWords:  treesit defun Prev PREV setq mapconcat prev
