;;; ts-inspect.el --- -*- lexical-binding: t -*-

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
