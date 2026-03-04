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

(defvar-local matlab-ts-mode--ei-errors-map nil) ;; Key LINENUM, if t on an error line.

(defun matlab-ts-mode--ei-mark-error-lines (error-node)
  "Add lines of ERROR-NODE to `matlab-ts-mode--ei-errors-map'."
  (let* ((error-start-pt (treesit-node-start error-node))
         (error-end-pt (treesit-node-end error-node))
         (error-start-linenum (line-number-at-pos error-start-pt))
         (error-end-linenum (line-number-at-pos error-end-pt)))
    (cl-loop
     for linenum from error-start-linenum to error-end-linenum do
     (puthash linenum t matlab-ts-mode--ei-errors-map))))

(defvar matlab-ts-mode--ei-all-nodes-query (when (treesit-available-p)
                                             (treesit-query-compile 'matlab '(_ @n))))

(defun matlab-ts-mode--ei-line-nodes-in-region (beg end)
  "Get line nodes in region BEG to END.
Line nodes are typically leaf nodes except for nodes like strings which
are not leaf nodes.  Returns list with elements (NODE
. MODIFIED-NODE-TYPE) where MODIFIED-NODE-TYPE is the NODE type
adjusted.  For example, when NODE parent is a string and NODE is the
string start characters, we return the parent string node.  Another
example: when NODE is a \"+\" and parent is a unary_operator, we return
MODIFIED-NODE-TYPE to be unary-op even though the node type is \"+\"."

  (setq matlab-ts-mode--ei-errors-map (make-hash-table))

  (matlab-ts-mode--ei-fast-back-to-indentation)
  (let ((region-nodes (treesit-query-capture (treesit-buffer-root-node 'matlab)
                                             matlab-ts-mode--ei-all-nodes-query beg end t))
        dimensions-node ;; we use this to track the "number" and ":" with in properties dimension
        line-nodes)

    (dolist (node region-nodes)
      (let ((node-type (treesit-node-type node)))

        (cond
         ;; Case: ERROR node
         ((string= node-type "ERROR")
          (matlab-ts-mode--ei-mark-error-lines node)
          (setq node nil))

         ;; Case: \n - ignore these, we don't pad them or anything
         ((string= node-type "\n")
          (setq node nil))

         ;; Case: track when in a property dimensions node
         ((string= node-type "dimensions")
          (setq node nil
                dimensions-node node))

         ;; Case: property dimensions
         ;;   foo1 (1, :) {mustBeNumeric, mustBeReal} = [0, 0, 0];
         ((and dimensions-node (or (string= node-type "number") (string= node-type ":")))
          (setq node-type "prop-dim"))

         ;; Case: lambda:     @(x) ((ischar(x) || isstring(x)))
         ;;                      ^
         ;;       properties: foo1 (1, :) {mustBeNumeric, mustBeReal} = [0, 0, 0];
         ;;                             ^
         ((string= node-type ")")
          (setq dimensions-node nil) ;; close of dimension-node or other close paren
          (let* ((parent (treesit-node-parent node))
                 (parent-type (treesit-node-type parent)))
            (if (string= parent-type "lambda")
                (setq node-type "lambda-)")
              (if (string= parent-type "dimensions")
                  (setq node-type "dim-)")))))

         ;; Case: parts of a string to ignore ("), ('), "string_content"
         ((or (string= node-type "\"") (string= node-type "'") (string= node-type "string_content"))
          (setq node nil))

         ;; Case: string ("double-quote-string" or 'single-quote-string')
         ((string= node-type "string")
          nil)

         ;; Case: prop-id, prop-class-id, enum-id
         ((string= node-type "identifier")
          (let* ((parent (treesit-node-parent node))
                 (parent-type (treesit-node-type parent)))
            (cond ((string= parent-type "property") ;; propertyWithOutDot?
                   (if (equal (treesit-node-child parent 0) node)
                       (setq node-type "prop-id")
                     (setq node-type "prop-class-id")))
                  ((string= parent-type "property_name") ;; property.nameWithDot?
                   (if (equal (treesit-node-child (treesit-node-parent parent) 0) parent)
                       (setq node-type "prop-id")
                     (setq node-type "prop-class-id")))
                  ((string= parent-type "enum")
                   (setq node-type "enum-id")))))

         ;; Case: unary operator sign, + or -, e.g. [0 -e] or g = - e
         ((or (string= node-type "+") (string= node-type "-"))
          (let* ((parent (treesit-node-parent node))
                 (parent-type (treesit-node-type parent)))
            (when(and (string= parent-type "unary_operator")
                      (equal (treesit-node-child parent 0) node))
              (setq node-type "unary-op"))))

         ;; Case: super-class constructor call
         ;;  obj@myUtils.super;
         ((string= node-type "@")
          (let* ((parent (treesit-node-parent node))
                 (parent-type (treesit-node-type parent)))
            (when (string= parent-type "function_call")
              (setq node-type "@-fcn-call"))))

         ;; Case: events, enumeration, methods
         ((string-match-p (rx bos (or "events" "enumeration" "methods" "arguments") eos) node-type)
          (let* ((parent (treesit-node-parent node))
                 (parent-type (treesit-node-type parent)))
            (when (string= parent-type "identifier")
              ;; TopTester: electric_indent_inspect_keyword_commands.m
              ;; TopTester: electric_indent_inspect_keyword_commands2.m
              (setq node-type (concat node-type "-fcn")))))

         ;; Case: arguments fcn keyword: arguments (1, :) {mustBeNumeric}
         ;;                                        ^
         ((string= node-type "(")
          (let* ((parent (treesit-node-parent node))
                 (parent-type (treesit-node-type parent)))
            (when (string= parent-type "dimensions")
              (setq node-type "dim-("))))

         ((not (= (treesit-node-child-count node) 0)) ;; non-leaf node?
          (setq node nil)))

        (when node
          (push `(,node . ,node-type) line-nodes))))

    (reverse line-nodes)))

(cl-defun matlab-ts-mode--ei-move-to-and-get-node-info (line-nodes-loc)
  "Move to and return (UPDATED-LOC NODE MODIFIED-NODE-TYPE).
Assumes point is at end of an existing node or at beginning of line.
LINE-NODES-LOC is a location within line-nodes that is managed by
this function.

UPDATED-LOC is the cdr location in LINE-NODES, i.e. following is t

  (let* ((pair (car UPDATED-LOC))
         (node (car pair))
     (and (>= (point) (treesit-node-start node)
          (>= (point) (treesit-node-start node))))))

Returned NODE and MODIFIED-NODE-TYPE will be nil if no next node before
end-of-line.  MODIFIED-NODE-TYPE is computed by
`matlab-ts-mode--ei-line-nodes-in-region' and is used in
`matlab-ts-mode--ei-spacing'."

  ;; Move point to next node start
  (when (looking-at "[ \t]")
    (if (re-search-forward "[^ \t]" nil (pos-eol))
        (backward-char)
      (cl-return-from matlab-ts-mode--ei-move-to-and-get-node-info (list line-nodes-loc))))

  ;; TODO - this doesn't handle what about the hidden ';' nodes

  (let* ((head-pair (car line-nodes-loc))
         (updated-loc line-nodes-loc)
         (pt (point)))
    
    (cl-loop while (let ((node (car head-pair)))
                     (and node
                          (or (< pt (treesit-node-start node))
                              (>= pt (treesit-node-end node)))))
             do
             (setq updated-loc (cdr updated-loc)
                   head-pair (car updated-loc)))

    (list updated-loc (car head-pair) (cdr head-pair))))

(defun prof-matlab-ts-mode--ei-line-nodes-region (arg)
  "Profile `matlab-ts-mode--ei-line-nodes-in-region'.
With prefix ARG, report elapsed time without profiling."
  (interactive "P")
  (when (not (eq major-mode 'matlab-ts-mode))
    (user-error "Buffer %s major-mode is not matlab-ts-mode" (buffer-name)))
  (let ((beg-pt (point-min))
        (end-pt (point-max))
        (start-time (current-time)))
    (when (not arg)
      (when (profiler-running-p)
        (profiler-stop))
      (profiler-start 'cpu))
    (unwind-protect
        (let* ((line-nodes (matlab-ts-mode--ei-line-nodes-in-region beg-pt end-pt))
               (line-nodes-loc line-nodes))
          (save-excursion
            (goto-char (point-min))
            (while (not (eobp))
              (if (looking-at "[ \t]*$")
                  (forward-line)
                (let ((tuple (matlab-ts-mode--ei-move-to-and-get-node-info line-nodes-loc)))
                  (setq line-nodes-loc (nth 0 tuple))
                  (let ((node (nth 1 tuple))
                        (node-type (nth 2 tuple)))
                    (message "Line %d:%d, node-type: %s, node: %S"
                             (line-number-at-pos) (current-column) node-type node)
                    (goto-char (min (treesit-node-end node) (pos-eol)))))))))
      (when (not arg)
        (profiler-stop)
        (profiler-report)))
    (message "%s" (matlab-ts-mode--ei-elapsed-time start-time))))

;; Multiple queries is slower than one large query
;; -----
;; (defun matlab-ts-mode--ei-nodes-in-line ()
;;   "Get leave nodes in current line."
;;   (matlab-ts-mode--ei-fast-back-to-indentation)
;;   (let ((line-nodes (treesit-query-capture (treesit-buffer-root-node)
;;                                            matlab-ts-mode--ei-all-nodes-query (point) (pos-eol) t))
;;         line-leaf-nodes)
;;     (dolist (node line-nodes)
;;       (when (= (treesit-node-child-count node) 0)
;;         (push node line-leaf-nodes)))
;;     (reverse line-leaf-nodes)))
;;
;; (defun prof-matlab-ts-mode--ei-nodes-in-line (arg)
;;   "Profile `matlab-ts-mode--ei-nodes-in-line'.
;; This profiles the current `matlab-ts-mode' buffer.
;; With prefix ARG, report elapsed time without profiling."
;;   (interactive "P")
;;   (when (not (eq major-mode 'matlab-ts-mode))
;;     (user-error "Buffer %s major-mode is not matlab-ts-mode" (buffer-name)))
;;   (goto-char (point-min))
;;   (let ((start-time (current-time))
;;         (count 0))
;;     (when (not arg)
;;       (profiler-start 'cpu))
;;     (unwind-protect
;;         (while (not (eobp))
;;           (let ((line-leaf-nodes (matlab-ts-mode--ei-nodes-in-line)))
;;             (setq count (+ count (length line-leaf-nodes))))
;;           (forward-line))
;;       (when (not arg)
;;         (profiler-stop)
;;         (profiler-report)))
;;     (message "Found %d leaf nodes. %s" count
;;              (matlab-ts-mode--ei-elapsed-time start-time))))

(provide 'prof-matlab-ts-mode--ei-capture)
;;; prof-matlab-ts-mode--ei-capture.el ends here

;; LocalWords:  SPDX gmail defun treesit dolist setq isstring bos eos eol eobp LINENUM linenum cdr
;; LocalWords:  puthash
