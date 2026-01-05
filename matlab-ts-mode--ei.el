;;; matlab-ts-mode--ei.el --- MATLAB electric indent -*- lexical-binding: t -*-

;; Version: 8.0.0
;; URL: https://github.com/mathworks/Emacs-MATLAB-Mode
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Author: John Ciolfi <john.ciolfi.32@gmail.com>
;; Created: Jul-7-2025
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
;;
;; Electric indent for matlab-ts-mode

;;; Code:

(require 'treesit)

(defvar matlab-ts-mode--array-indent-level)
(defvar matlab-ts-mode--indent-level)
(declare-function matlab-ts-mode "matlab-ts-mode.el")

(defgroup matlab-ts nil
  "MATLAB(R) tree-sitter mode."
  :prefix "matlab-ts-mode-"
  :group 'languages)

(defcustom matlab-ts-mode-electric-indent t
  "*If t, indent (format) language elements within code.
- Canonicalize language elements spacing
     Example                        |  Result
     -----------------------------  |  -----------------------------
     a = b+ c *d ;                  |  a = b + c * d;

- Align consecutive assignments
     Example                        |  Result
     -----------------------------  |  -----------------------------
     width = 100;                   |  width  = 100;
     length = 200;                  |  length = 200;
     area=width * length;           |  area   = width * length;

- Align trailing comments
     Example                        |  Result
     -----------------------------  |  -----------------------------
     a = myFcn1(1, 2); % comment 1  |  a = myFcn1(1, 2); % comment 1
     a = a + 5; % comment 2         |  a = a + 2;        % comment 2

- Align matrix columns
     Example                        |  Result
     -----------------------------  |  -----------------------------
     m = [2,4000                    |  m = [   2, 4000
            3000,1]                 |       3000,    1]

- Align properties
   Example:
      TODO
   is indented as:
      TODO"
  :type 'boolean)

(defvar matlab-ts-mode--electric-indent-verbose nil)
(defvar matlab-ts-mode--electric-indent-assert nil)

(defvar matlab-ts-mode--ei-keywords-re
  (rx bos (or "arguments" ;; not technically a keyword (can be a variable), but treat as a keyword
              "break"
              "case"
              "catch"
              "classdef"
              "continue"
              "else"
              "elseif"
              "end"
              "enumeration"
              "events"
              "for"
              "function"
              "get." ;; when used in a classdef method, e.g. function value = get.propName(obj)
              "global"
              "if"
              "methods"
              "otherwise"
              "parfor"
              "persistent"
              "properties"
              "return"
              "set." ;; when used in a classdef method, e.g. function obj = set.propName(obj, val)
              "spmd"
              "switch"
              "try"
              "while")
      eos))

(defvar matlab-ts-mode--ei-pad-op-re
  (rx bos (or "+" "-" "*" "/" ".*" "./" "\\"
              "==" "~=" ">" ">=" "<" "<="
              "&" "|" "&&" "||"
              "="
              "^" ".^"
              "'" ".'"
              ":")
      eos))

(defvar matlab-ts-mode--ei-0-after-re
  (rx bos (or "[" "{" "(" "~" "unary-op") eos))

(defvar matlab-ts-mode--ei-val-re (rx bos (or "identifier" "number") eos))

;; TODO optimize following by grouping together, also improve comments
(defvar matlab-ts-mode--ei-spacing
  ;; In a given line, we walk across the nodes adjusting spaces between NODE and NEXT-NODE to
  ;; have N-SPACES-BETWEEN them.
  ;;
  ;; NODE-RE                          NEXT-NODE-RE                                 N-SPACES-BETWEEN
  `(

    ("."                              ,(rx bos (or "comment" "line_continuation") eos)           1)


    ;; Case: property dimension
    ;;         foo1 (1, :) {mustBeNumeric, mustBeReal} = [0, 0, 0];
    ("."                              ,(rx bos "prop-dim" eos)                                   0)

    ;; Case;  obj@myUtils.super;
    ;; TopTester: tests/test-matlab-ts-mode-electric-indent-files/electric_indent_call_super.m
    ("."                              ,(rx bos "@-fcn-call" eos)                                 0)
    (,(rx bos "@-fcn-call" eos)       "."                                                        0)

    ;; Case: a.?b, M', M.'
    ("."                              ,(rx bos (or ".?" "'" ".'") eos)                           0)
    (,(rx bos ".?" eos)               "."                                                        0)

    ;; Case: power and transpose: a^b a.^b
    (,matlab-ts-mode--ei-val-re       (,(rx bos (or "^" ".^") eos) . ,matlab-ts-mode--ei-val-re) 0)

    ;; Case: anything followed by ",", etc.: [a, b]
    ("."                              ,(rx bos (or "," ";" ".") eos)                             0)

    ;; Case lambda: @(x), metaclass operator, ?
    (,(rx bos (or "@" "?") eos)       "."                                                        0)

    ;; Case: anything after a ".", e.g. foo.bar, s.(fieldName)
    (,(rx bos "." eos)                "."                                                        0)

    (,(rx bos (or "," ";" "command_argument" "command_name") eos)       "."                      1)

    (,matlab-ts-mode--ei-0-after-re   "."                                                        0)

    (,(rx bos "]" eos)                ,(rx bos (or "," ";") eos)                                 0)
    (,(rx bos "]" eos)                ,(rx bos "[" eos)                                          1)
    ("."                              ,(rx bos (or "]" ")" "}") eos)                             0)

    ;; Case: ") identifier" as in: propName (1, 1) double
    ;;       arguments:  g (1,1) {mustBeNumeric, mustBeReal}
    ;;       @(x) ((ischar(x) || isstring(x)));
    (,(rx bos ")" eos)                ,(rx bos (or "identifier" "{" "(") eos)                    1)

    ;; Case: property identifier: propName (1,1) double
    (,(rx bos "property-id" eos)      "."                                                        1)

    ;; Case: padded operators, e.g.: a || b
    (,matlab-ts-mode--ei-pad-op-re    "."                                                        1)
    ("."                              ,matlab-ts-mode--ei-pad-op-re                              1)

    ;; Case: string followed by anything, e.g. ["string1" foo(1)]
    (,(rx bos "string" eos)           "."                                                        1)

    ;; Case: anything before string, e.g. [foo(1) "string1"]
    ("."                              ,(rx bos "string" eos)                                     1)

    ;; Case: c3 = {b [c '%']};
    ("."                              ,(rx bos "[" eos)                                          1)

    (,(rx bos "identifier" eos)       ,(rx bos (or "(" "{") eos)                                 0)
    (,(rx bos "identifier" eos)       "."                                                        1)

    ;; Case: number in matrix: [123 456]
    (,(rx bos "number" eos)           "."                                                        1)

    ;; Case: subclass
    (,(rx bos "<" eos)                "."                                                        1)

    ;; Case: keywords, e.g. if condition
    (,matlab-ts-mode--ei-keywords-re  "."                                                        1)

    ;; Case: c = {['freq' '%'] num2str(2)};
    (,(rx bos "]" eos)                "."                                                        1)

    ;; Case: c4{1} = [1 2; 3 4];
    ;;       v4 = [c4{1}(1,1), c4{1}(1,1)];
    (,(rx bos "}" eos)                ,(rx bos "(" eos)                                          0)

    ;; Case: ")": m3 = uint8([ones(20,1); 2*ones(8,1)]);
    ;;                                 ^  ^
    ;; Case: c3 = {{[17.50 0] [17.50 0]} {[120 0] [120 20]}};
    ;;                                 ^ ^
    (,(rx bos (or ")" "}") eos)       "."                                                        1)
    ))

(cl-defun matlab-ts-mode--ei-move-to-and-get-node ()
  "Move to and return node.
Will return nil if no next node before end-of-line.
Assumes point is at of current node or beginning of line."
  ;; Move point to first non-whitespace char
  (let ((eol (line-end-position)))
    (when (looking-at "[ \t]")
      (when (not (re-search-forward "[^ \t]" eol t))
        (cl-return-from matlab-ts-mode--ei-move-to-and-get-node))
      (backward-char))

    (let ((node (treesit-node-at (point)))
          node-type)

      ;; Consider [[1,2];[3,4]] when point is on semicolon, node will be the prior "]" because the
      ;; semicolon is an ignored node, so move forward to get to the "[" after the semicolon.
      (while (and (not (eobp))
                  (let ((node-start (treesit-node-start node)))
                    (and (>= node-start (line-beginning-position))
                         (< node-start (point)))))
        (forward-char)
        (setq node (treesit-node-at (point))))

      ;; Don't go past end-of-line point
      (when (or (equal "\n" (treesit-node-type node))
                (> (treesit-node-start node) eol)
                ;; When we get to EOL and in error context, node start will be on an earlier line
                ;;           x = [
                ;;  TAB>           1  ,   2  ;
                (< (treesit-node-start node) (line-beginning-position)))
        (goto-char eol)
        (setq node nil))

      (when node
        (setq node-type (treesit-node-type node))
        (let* ((parent (treesit-node-parent node))
               (parent-type (treesit-node-type parent)))
          (cond
           ;; Use string and not the elements of the string
           ((equal parent-type "string")
            (setq node parent
                  node-type parent-type))

           ;; convert property identifier to property-id node-type
           ((and (equal node-type "identifier")
                 (or
                  ;; propertyWithOutDot?
                  (and (equal parent-type "property")
                       (equal (treesit-node-child parent 0) node))
                  ;; property.nameWithDot?
                  (and (equal parent-type "property_name")
                       (equal (treesit-node-child (treesit-node-parent parent) 0) parent))))
            (setq node-type "property-id"))

           ;; Unary operator sign, + or -, e.g. [0 -e] or g = - e
           ((and (equal parent-type "unary_operator")
                 (equal (treesit-node-child parent 0) node))
            (setq node-type "unary-op"))

           ;; Super-class constructor call
           ;;  obj@myUtils.super;
           ((and (equal node-type "@")
                 (equal parent-type "function_call"))
            (setq node-type "@-fcn-call"))

           ;; Property dimensions
           ;;   foo1 (1, :) {mustBeNumeric, mustBeReal} = [0, 0, 0];
           ((and (or (equal node-type "number") (equal node-type ":"))
                 (or (equal parent-type "dimensions")
                     (and (equal parent-type "spread_operator")
                          (equal (treesit-node-type (treesit-node-parent parent))
                                 "dimensions"))))
            (setq node-type "prop-dim"))
           )))
      (cons node node-type))))

(defun matlab-ts-mode--ei-assert-match (line-node-types)
  "Assert that LINE-NODE-TYPES string matches current line."
  (back-to-indentation)
  (let (curr-line-node-types)
    (cl-loop
     while (< (point) (line-end-position))
     do
     (let* ((pair (matlab-ts-mode--ei-move-to-and-get-node))
            (node (or (car pair)
                      (cl-return)))
            (node-type (cdr pair)))
       (setq curr-line-node-types
             (matlab-ts-mode--ei-update-line-node-types curr-line-node-types
                                                        node node-type))
       (let ((node-end (treesit-node-end node)))
         (if (< node-end (line-end-position))
             (goto-char node-end)
           (goto-char (line-end-position))))))

    (when (not (string= curr-line-node-types line-node-types))
      (error "Assert: line-node-types mismatch \"%s\" !EQ \"%s\" at line %d in %s"
             curr-line-node-types line-node-types (line-number-at-pos (point)) (buffer-name)))))

(defun matlab-ts-mode--ei-concat-line (ei-line node extra-chars &optional n-spaces-to-append)
  "Return concat EI-LINE with NODE text.
NODE-END is the NODE end accounting for ignored nodes (semicolons).
EXTRA-CHARS are appended to EL-LINE.
N-SPACES-TO-APPEND is the number of spaces to append between nodes."

  (let* ((node-end (treesit-node-end node))
         (last-pt (if (< node-end (line-end-position)) node-end (line-end-position))))
    (concat ei-line
            (buffer-substring (treesit-node-start node) last-pt)
            extra-chars
            (if (not n-spaces-to-append) ;; last node?
                ;; Add trailing whitespace when in an ERROR node. Consider
                ;;    switch a
                ;;      case                    ;; One trailing whitespace
                ;;    end
                ;; TopTester: electric_indent_xr_switch.m
                (when (and (treesit-parent-until node (rx bos "ERROR" eos))
                           (< last-pt (line-end-position)))
                  (save-excursion
                    (end-of-line)
                    (when (re-search-backward "[^ \t]" (line-beginning-position) t)
                      (forward-char)
                      (when (not (= (point) (line-end-position)))
                        (buffer-substring (point) (line-end-position))
                        ))))
              (when (> n-spaces-to-append 0)
                (make-string n-spaces-to-append ? ))))))

(defun matlab-ts-mode--ei-no-elements-to-indent ()
  "Return t if no elements in the current line to indent.
Assumes that current point is at `back-to-indentation'."
  (or
   ;; (1) Empty line?
   (not (looking-at "[^ \t\n\r]"))
   ;; (2) Comment line? Nothing to indent in line if it's a comment line.
   (let ((first-node-type (or (treesit-node-type (treesit-node-at (point))) "")))
     (string-match-p (rx bos (or "line_continuation" "comment") eos) first-node-type))
   ;; (3) Syntax error *within* the line? If error node covers whole line, assume nodes in
   ;;     line are good, i.e. electric indent the line.
   (let ((beg (line-beginning-position))
         (end (line-end-position))
         (capture-errors (treesit-query-capture (treesit-buffer-root-node) '((ERROR) @e))))
     (cl-loop
      for capture-error in capture-errors do
      (let* ((error-node (cdr capture-error))
             (error-start (treesit-node-start error-node))
             (error-end (treesit-node-end error-node)))
        (when (and (> error-start beg)
                   (< error-end end))
          (cl-return t))
        )))))

(defun matlab-ts-mode--ei-node-extra-chars (node-end next-node-start)
  "Get extra chars after NODE-END and before NEXT-NODE-START."

  (let ((extra-chars ""))
    ;; Handle ignored characters, e.g. ";" in matrices where node="]", next-node="["
    ;;   [[1, 2]; [3, 4]]
    ;;         ^  ^
    (goto-char node-end)
    (when (or ;; [[1,2];[3,4]]?
           (and (< node-end next-node-start)
                (looking-at "[^ \t]"))
           ;; [[1,2] ; [3,4]]?
           ;; or a multiline matrix:
           ;; x = [ 1 , 2 ;
           (save-excursion (and (when (re-search-forward "[^ \t]" next-node-start t)
                                  (backward-char)
                                  t)
                                (< (point) next-node-start))))
      (while (< (point) next-node-start)
        (while (and (< (point) next-node-start)
                    (looking-at "[^ \t]"))
          (setq extra-chars (concat extra-chars (match-string 0)))
          (forward-char)
          (setq node-end (point)))
        (re-search-forward "[ \t]+" next-node-start t)))
    extra-chars))

(defun matlab-ts-mode--ei-update-line-node-types (line-node-types node node-type)
  "Append NODE-TYPE of NODE to LINE-NODE-TYPES."
  (if (and (string= node-type ";")
           (string= (treesit-node-type (treesit-node-parent node)) "matrix"))
      ;; Ignore ';' row separator in matrix because these may be ignored by tree-sitter
      line-node-types
    (concat line-node-types (when line-node-types " ") node-type)))

(cl-defun matlab-ts-mode--ei-get-new-line (&optional start-node start-offset)
  "Get new line content with element spacing adjusted.
Optional START-NODE and START-OFFSET are used to compute new pt-offset,
the point offset in the line used to restore point after updating line.
Note, new line content may be same as current line.  Also computes
line-node-types which is a string containing the line node types.
Returns electric indent info, ei-info,
  (list NEW-LINE-CONTENT PT-OFFSET LINE-NODE-TYPES FIRST-NODE-IN-LINE)
or nil."
  (save-excursion
    (back-to-indentation)
    (when (matlab-ts-mode--ei-no-elements-to-indent)
      (cl-return-from matlab-ts-mode--ei-get-new-line))

    ;; Compute ei-line, the electric indented line content
    (let* (pt-offset ;; used in restoring point
           (ei-line (buffer-substring (line-beginning-position) (point)))
           (pair (matlab-ts-mode--ei-move-to-and-get-node))
           (node (or (car pair)
                     (cl-return-from matlab-ts-mode--ei-get-new-line)))
           (node-type (cdr pair))
           (first-node node)
           line-node-types
           next2-pair ;; used when we have: (NODE-RE (NEXT-NODE-RE NEXT2-NODE-RE) N-SPACES-BETWEEN)
           next2-n-spaces-between)

      (cl-loop
       while (and (< (point) (line-end-position))
                  (< (treesit-node-end node) (line-end-position)))
       do
       (let* ((next-pair (progn
                           (goto-char (treesit-node-end node))
                           (or next2-pair
                               (matlab-ts-mode--ei-move-to-and-get-node))))
              (next-node (let ((candidate-node (car next-pair)))
                           (when (not candidate-node)
                             (cl-return))
                           (when (= (treesit-node-start candidate-node)
                                    (treesit-node-end candidate-node))
                             ;; Syntax errors can result in empty nodes, so skip this line
                             ;; TopTester: electric_indent_empty_node_error.m
                             (cl-return-from matlab-ts-mode--ei-get-new-line))
                           candidate-node))
              (next-node-type (cdr next-pair))
              (n-spaces-between next2-n-spaces-between))

         (setq next2-pair nil
               next2-n-spaces-between nil)

         (when matlab-ts-mode--electric-indent-assert
           (setq line-node-types (matlab-ts-mode--ei-update-line-node-types line-node-types
                                                                         node node-type)))

         (when (not n-spaces-between)
           (cl-loop for tuple in matlab-ts-mode--ei-spacing do
                    (let* ((node-re (nth 0 tuple))
                           (next-spec (nth 1 tuple))
                           (next-node-re (if (listp next-spec) (car next-spec) next-spec))
                           (next2-node-re (when (listp next-spec) (cdr next-spec))))

                      (when (and (string-match-p node-re node-type)
                                 (string-match-p next-node-re next-node-type)
                                 (or (not next2-node-re)
                                     (save-excursion
                                       (goto-char (treesit-node-end next-node))
                                       (let* ((pair (matlab-ts-mode--ei-move-to-and-get-node))
                                              (next2-node-type
                                               (or (cdr pair)
                                                   (cl-return-from
                                                       matlab-ts-mode--ei-get-new-line))))

                                         (when (string-match-p next2-node-re next2-node-type)
                                           (setq next2-pair pair)
                                           (setq next2-n-spaces-between (nth 2 tuple))
                                           t)))))

                        (setq n-spaces-between (nth 2 tuple))
                        (when matlab-ts-mode--electric-indent-verbose
                          (message "-->ei-matched: %S for node=<\"%s\" %S> next-node=<\"%s\" %S>"
                                   tuple node-type node next-node-type next-node))
                        (cl-return)))))

         (when (not n-spaces-between)
           (error "Internal error, unhandled node <\"%s\" %S> and next-node <\"%s\" %S>"
                  node-type node next-node-type next-node))

         (let* ((node-end (treesit-node-end node))
                (next-node-start (treesit-node-start next-node))
                (extra-chars (matlab-ts-mode--ei-node-extra-chars node-end next-node-start)))
           ;; Update ei-line
           (when (equal start-node node)
             (setq pt-offset (+ (length ei-line) start-offset)))

           (setq ei-line (matlab-ts-mode--ei-concat-line ei-line node extra-chars n-spaces-between))

           (setq node next-node
                 node-type next-node-type)
           )))

      (when node
        (when (equal start-node node)
          (setq pt-offset (+ (length ei-line) start-offset)))
        (when matlab-ts-mode--electric-indent-assert
          (setq line-node-types (matlab-ts-mode--ei-update-line-node-types line-node-types
                                                                           node node-type)))
        (let ((extra-chars (matlab-ts-mode--ei-node-extra-chars
                            (min (treesit-node-end node) (line-end-position))
                            (line-end-position))))
          (setq ei-line (matlab-ts-mode--ei-concat-line ei-line node extra-chars))))

      (list ei-line pt-offset line-node-types first-node))))

(cl-defun matlab-ts-mode--ei-m-matrix-first-col-extra (matrix)
  "For MATRIX indent alignment, get first-col-extra."
  ;; For first-col-extra consider the following where matlab-ts-mode--array-indent-level is 2.
  ;; In this case first-col-extra will be 1.
  ;;   m = [
  ;;         1 2
  ;;         3 4
  ;;       ];
  (let ((first-col-extra (save-excursion
                           (goto-char (treesit-node-start matrix))
                           (forward-char) ;; step over the "["
                           (let ((found-element nil))
                             ;; found a matrix element?
                             (while (and (not found-element)
                                         (re-search-forward "[^ \t]" (line-end-position) t))
                               (backward-char)
                               (let ((node (treesit-node-at (point))))
                                 (when (not (string-match-p (rx bos (or "comment"
                                                                        "line_continuation")
                                                                eos)
                                                            (treesit-node-type node)))
                                   (setq found-element t))
                                 (goto-char (min (line-end-position)
                                                 (treesit-node-end node)))))
                             (if found-element 0 (1- matlab-ts-mode--array-indent-level))))))
    first-col-extra))

(cl-defun matlab-ts-mode--ei-m-matrix-col-widths (matrix first-col-extra &optional first-col-only)
  "Get multi-line MATRIX column widths adding in FIRST-COL-EXTRA to first column.
If optional FIRST-COL-ONLY is non-nil, then return only the width of the
first column in MATRIX.
Returns alist where each element in the alist is (COLUMN-NUM . WIDTH)"
  (let ((column-widths '())) ;; (alist-get column-num column-widths) ==> width
    (dolist (m-child (treesit-node-children matrix))
      (when (string= (treesit-node-type m-child) "row")
        (let ((column-num 0))
          (cl-loop
           for entry in (treesit-node-children m-child)
           do
           (when (not (string= (treesit-node-type entry) ","))
             (setq column-num (1+ column-num))
             (let ((width (or (alist-get column-num column-widths) 0))) ;; matrix element width
               (let ((el-width (- (treesit-node-end entry) (treesit-node-start entry))))
                 (when (> el-width width)
                   (if (= width 0)
                       (push `(,column-num . ,el-width) column-widths)
                     (setf (alist-get column-num column-widths) el-width)))))
             (when first-col-only
               (cl-return))
             )))))

    (when (> first-col-extra 0)
      (let ((col1-width (+ (alist-get 1 column-widths) first-col-extra)))
        (setf (alist-get 1 column-widths) col1-width)))

    column-widths))

(defun matlab-ts-mode--ei-get-m-matrix-row-in-line ()
  "Given point within a matrix assignment statement, return row node.
Note, nil may be returned when line is only a continuation, e.g.
   v = [1 2; ...
        ...
        3 4];
when on the 2nd continuation only line, nil is returned."
  (save-excursion
    (back-to-indentation)
    (let (row-node
          found-ans)
      (cl-loop
       while (not found-ans) do

       (let* ((node-at-pt (treesit-node-at (point)))
              (node node-at-pt))
         (while (and node
                     (not (string-match-p (rx bos (or "row" "matrix" "assignment") eos)
                                          (treesit-node-type node))))
           (setq node (treesit-node-parent node)))

         (if (and node
                  (string= (treesit-node-type node) "row"))
             (setq found-ans t
                   row-node node)
           (goto-char (min (treesit-node-end node-at-pt) (line-end-position)))
           (when (not (re-search-forward "[^ \t]" (line-end-position) t))
             (setq found-ans t)))))
      row-node)))

;; Internal variable that shouldn't be altered. It's used to avoid infinite recursion.
(defvar matlab-ts-mode--ei-align-enabled t)

;; This is used to cache matrix alignments for indent-region
;; It will be non-nil when called from indent-region.
(defvar matlab-ts-mode--ei-align-matrix-alist nil)

(defun matlab-ts-mode--ei-indent-matrix-in-tmp-buf (assign-node)
  "Insert ASSIGN-NODE in to current tmp-buf and indent.
Point is left at beginning of line containing the ASSIGN-NODE text."
  (let (assign-str
        n-levels)
    (with-current-buffer (treesit-node-buffer assign-node)
      (let* ((assign-start-pos (save-excursion (goto-char (treesit-node-start assign-node))
                                              (line-beginning-position)))
             (assign-end-pos (save-excursion (goto-char (treesit-node-end assign-node))
                                             (line-end-position)))
             (indent-spaces (- (treesit-node-start assign-node) assign-start-pos)))
        (setq assign-str (buffer-substring assign-start-pos assign-end-pos)
              n-levels (if (= (mod indent-spaces matlab-ts-mode--indent-level) 0)
                           (/ indent-spaces matlab-ts-mode--indent-level)
                         ;; else: not at a standard level so no need to add conditionals as the
                         ;; indent level will be corrected later.
                         0))))

    (cl-loop for level from 1 to n-levels do
             (insert "if 1\n"))

    (insert assign-str "\n")

    (cl-loop for level from 1 to n-levels do
             (insert "end\n"))

    (matlab-ts-mode)

    ;; Indent to adjust spacing among operators, but don't do other alignment items
    (let ((matlab-ts-mode--ei-align-enabled nil)
          ;; t-utils-test-indent captures messages using treesit--indent-verbose and we don't
          ;; want to capture the messages from this temp indent-region.
          (treesit--indent-verbose nil))
      (indent-region (point-min) (point-max)))


    (goto-char (point-min))
    (when (> n-levels 0)
      (forward-line n-levels))))

(cl-defun matlab-ts-mode--ei-align-line-in-m-matrix (assign-node ei-info)
  "Align current line with EI-INFO in a multi-line matrix of ASSIGN-NODE.
See `matlab-ts-mode--ei-get-new-line' for EI-INFO contents."

  ;; TopTester: test-matlab-ts-mode-electric-indent-files/electric_indent_matrix_cols.m
  (when matlab-ts-mode--ei-align-matrix-alist ;; Use cached value?
    (let ((ei-line (alist-get (line-number-at-pos) matlab-ts-mode--ei-align-matrix-alist)))
      (when ei-line
        (cl-return-from matlab-ts-mode--ei-align-line-in-m-matrix (cons ei-line (cdr ei-info))))))

  (let* ((assign-start-linenum (line-number-at-pos (treesit-node-start assign-node)))
         (tmp-buf-ei-linenum (1+ (- (line-number-at-pos) assign-start-linenum)))
         (tmp-buf-row-linenum (if matlab-ts-mode--ei-align-matrix-alist 1 tmp-buf-ei-linenum))
         (matrix-alist matlab-ts-mode--ei-align-matrix-alist))
    (with-temp-buffer
      (matlab-ts-mode--ei-indent-matrix-in-tmp-buf assign-node)

      (let* ((matrix-node (treesit-node-parent (treesit-search-subtree
                                                (treesit-buffer-root-node) (rx bos "[" eos) nil t)))
             (first-col-extra (matlab-ts-mode--ei-m-matrix-first-col-extra matrix-node))
             (column-widths (matlab-ts-mode--ei-m-matrix-col-widths matrix-node first-col-extra)))

        ;; Move to the line of interest when we called from matlab-ts-mode--treesit-indent,
        ;; otherwise calculate all matrix rows for indent-region.
        (when (and (not matrix-alist)
                   (> tmp-buf-ei-linenum 1))
          (forward-line (1- tmp-buf-ei-linenum)))

        (while (not (eobp)) ;; Adjust column widths
          (back-to-indentation)
          (let* ((row-node (matlab-ts-mode--ei-get-m-matrix-row-in-line))
                 (indent-start-pt (point))
                 ;; line content does not have leading indent-level spaces
                 (content (buffer-substring indent-start-pt (line-end-position)))
                 (ei-line (buffer-substring (line-beginning-position) (line-end-position)))
                 n-spaces)
            (when row-node
              (let* ((col-num (length column-widths))
                     (pt-offset (nth 1 ei-info))
                     (matrix-offset (save-excursion
                                      (goto-char (treesit-node-start matrix-node))
                                      (1+ (- (point) (line-beginning-position)))))
                     (indent-offset (or (string-match-p "[^ \t]+" ei-line)
                                        (error "Assert: no offset"))))

                (when (< matrix-offset indent-offset)
                  (when pt-offset
                    (setq pt-offset (- pt-offset (- indent-offset matrix-offset))))
                  (setq indent-offset matrix-offset))

                (dolist (element (reverse (treesit-node-children row-node)))
                  (when (not (string= (treesit-node-type element) ",")) ;; at a column?
                    (let ((width (or (alist-get col-num column-widths)
                                     (error "Assert: no col width")))
                          (curr-width (- (treesit-node-end element) (treesit-node-start element))))
                      (setq n-spaces (- width curr-width))
                      (when (< curr-width width)
                        (let ((offset (- (treesit-node-start element) indent-start-pt)))
                          (when (and pt-offset
                                     (or (= indent-start-pt (point-min))
                                         (<= (+ offset matrix-offset) pt-offset)))
                            (setq pt-offset (+ pt-offset n-spaces)))
                          (setq content (concat (substring content 0 offset)
                                                (make-string n-spaces ? )
                                                (substring content offset)))))
                      (setq col-num (1- col-num)))))

                (setq ei-line (concat (substring ei-line 0 indent-offset) content))
                (when (= tmp-buf-row-linenum tmp-buf-ei-linenum)
                  (setq ei-info (list ei-line pt-offset (nth 2 ei-info) (nth 3 ei-info))))))

            (when matrix-alist
              (let* ((buf-linenum (1- (+ assign-start-linenum tmp-buf-row-linenum))))
                (push `(,buf-linenum . ,ei-line) matrix-alist)))

            (if (not matrix-alist)
                (goto-char (point-max))
              (forward-line)
              (setq tmp-buf-row-linenum (1+ tmp-buf-row-linenum)))))))

    (when matrix-alist
      (setq matlab-ts-mode--ei-align-matrix-alist matrix-alist)))
  ;; ei-info for current line
  ei-info)

(cl-defun matlab-ts-mode--ei-matrix-ends-on-line (matrix)
  "Does MATRIX end on a line by itself?"
  (save-excursion
    (goto-char (treesit-node-end matrix))
    (while (re-search-forward "[^ \t]" (line-end-position) t)
      (backward-char)
      (let ((node (treesit-node-at (point))))
        (when (not (string-match-p (rx bos (or "," ";" "comment" "line_continuation") eos)
                                   (treesit-node-type node)))
          (cl-return-from matlab-ts-mode--ei-matrix-ends-on-line))
        (goto-char (treesit-node-end node)))))
  t)

(cl-defun matlab-ts-mode--ei-is-m-matrix (matrix)
  "Is MATRIX node a multi-line matrix?
We define a a multi-line matrix has one row per line and more than one
column."
  (let ((start-line (line-number-at-pos (treesit-node-start matrix)))
        (end-line (line-number-at-pos (treesit-node-end matrix)))
        (n-rows 0)
        n-cols)
    (when (and (> end-line start-line) ;; multi-line matrix?
               (matlab-ts-mode--ei-matrix-ends-on-line matrix)
               (not (treesit-search-subtree matrix (rx bos "ERROR" eos) nil t)))
      (dolist (child (treesit-node-children matrix))
        (let ((child-type (treesit-node-type child)))
          (cond
           ((string= child-type "row")
            (let ((row-start-line-num (line-number-at-pos (treesit-node-start child))))
              ;; Return nil if row not on one line
              (when (not (= row-start-line-num (line-number-at-pos (treesit-node-end child))))
                (cl-return-from matlab-ts-mode--ei-is-m-matrix))
              ;; Return nil if more than one row one the line
              (let ((next-node (treesit-node-next-sibling child)))
                (when (and (string= (treesit-node-type next-node) "row")
                           (= row-start-line-num (line-number-at-pos
                                                  (treesit-node-start next-node))))
                  (cl-return-from matlab-ts-mode--ei-is-m-matrix)))
              ;; Return nil if row contains sub-matrices
              (when (treesit-search-subtree child (rx bos (or "[" "]") eos) nil t)
                (cl-return-from matlab-ts-mode--ei-is-m-matrix))

              (setq n-rows (1+ n-rows))

              ;; Count the columns
              (let ((n-cols-in-row 0))
                (dolist (el (treesit-node-children child))
                  (when (not (string= (treesit-node-type el) ","))
                    (setq n-cols-in-row (1+ n-cols-in-row))))
                (if (not n-cols)
                    (setq n-cols n-cols-in-row)
                  (if (not (= n-cols n-cols-in-row))
                      (cl-return-from matlab-ts-mode--ei-is-m-matrix))))
              ))
           ;; Case unexpected matrix child node
           ((not (string-match-p (rx bos (or "[" "]" "comment" "line_continuation") eos)
                                 child-type))
            (error "Assert: unexpected matrix child %S" child))))))
    ;; Matrix with more than one row and more than one column where each row is on its own line?
    (and (> n-rows 1) (>= n-cols 1))))

(defun matlab-ts-mode--ei-is-assign (first-node type)
  "Is FIRST-NODE of line for an assignment that matches TYPE?
TYPE can be \\='single-line or \\='multi-line-matrix.  When
\\='multi-line-matrix, the assignment is to a matrix with two or more
rows and two or more columns, where each row is on its own line.  The
assignment node is return or nil."
  (let ((assign-node (treesit-node-parent first-node)))
    (while (and assign-node
                (not (string= (treesit-node-type assign-node) "assignment")))
      (setq assign-node (treesit-node-parent assign-node)))
    (when assign-node
      (save-excursion
        (beginning-of-line)
        (when (re-search-forward "=" (line-end-position) t)
          (backward-char)
          (let ((eq-node (treesit-node-at (point))))
            ;; First "=" must be an assignment (assumptions elsewhere require this).
            (when (and (equal (treesit-node-type eq-node) "=")
                       (equal (treesit-node-type (treesit-node-parent eq-node)) "assignment"))
              (cond
               ;; Single-line assignment? Example: v1 = [1, 2];
               ((eq type 'single-line)
                (when (<= (treesit-node-end assign-node) (line-end-position))
                  assign-node))

               ;; Multi-line matrix assignment? Example: m1 = [1 2
               ;;                                              3 4];
               ((eq type 'multi-line-matrix)
                (goto-char (treesit-node-end eq-node))
                (let ((next-node (treesit-node-next-sibling eq-node)))
                  (while (equal (treesit-node-type next-node) "line_continuation")
                    (setq next-node (treesit-node-next-sibling next-node)))
                  (when (and (equal (treesit-node-type next-node) "matrix")
                             (matlab-ts-mode--ei-is-m-matrix next-node))
                    assign-node)))
               (t
                (error "Assert: bad type %S" type))))))))))

(defun matlab-ts-mode--ei-point-in-m-matrix (ei-info)
  "Are we in a multi-line matrix?
See `matlab-ts-mode--ei-get-new-line' for EI-INFO contents.  Returns
mat-info a (list matrix-node n-rows n-cols) if in a multi-line matrix."
  (let* ((first-node-in-line (nth 3 ei-info))
         (parent (treesit-node-parent first-node-in-line)))
    (while (and parent
                (not (string= (treesit-node-type parent) "assignment")))
      (setq parent (treesit-node-parent parent)))
    (when parent ;; In an assignment?
      (save-excursion
        (goto-char (treesit-node-start parent))
        (beginning-of-line)
        (back-to-indentation)
        (let ((first-node (treesit-node-at (point))))
          (matlab-ts-mode--ei-is-assign first-node 'multi-line-matrix))))))

(defun matlab-ts-mode--ei-assign-offset (ei-line)
  "Get the assignment offset from the indent-level in EI-LINE."
  (let* ((first-char-offset (or (string-match-p "[^ \t]" ei-line) (error "Assert: no first char")))
         (offset (- (or (string-match-p "=" ei-line) (error "Assert: no ="))
                    first-char-offset)))
    offset))

;; This is used to cache aligned assignments for indent-region
(defvar-local matlab-ts-mode--ei-align-assign-alist nil)

(defun matlab-ts-mode--ei-align-assignments (ei-info)
  "Update EI-INFO to align assignments.
See `matlab-ts-mode--ei-get-new-line' for EI-INFO contents."
  (let ((first-node-in-line (nth 3 ei-info)))
    (when (matlab-ts-mode--ei-is-assign first-node-in-line 'single-line)
      (let* ((ei-line (nth 0 ei-info))
             (line-assign-offset (matlab-ts-mode--ei-assign-offset ei-line))
             assign-offset
             line-nums
             line-start-pt)

        (when (or (not matlab-ts-mode--ei-align-assign-alist)
                  (not (setq assign-offset (alist-get (line-number-at-pos)
                                                      matlab-ts-mode--ei-align-assign-alist))))
          (setq assign-offset line-assign-offset)
          (setq line-nums `(,(line-number-at-pos)))
          (save-excursion
            (beginning-of-line)
            (setq line-start-pt (point))

            ;; Look backwards and then forwards for single-line assignments
            (cl-loop
             for direction in '(-1 1) do
             (goto-char line-start-pt)
             (cl-loop
              while (not (if (= direction -1) (bobp) (eobp))) do
              (forward-line direction)
              (let* ((l-info (matlab-ts-mode--ei-get-new-line))
                     (l-first-node (nth 3 l-info)))
                (if (and l-first-node
                         (matlab-ts-mode--ei-is-assign l-first-node 'single-line))
                    (let ((l-offset (matlab-ts-mode--ei-assign-offset (nth 0 l-info))))
                      (push (line-number-at-pos) line-nums)
                      (when (> l-offset assign-offset)
                        (setq assign-offset l-offset)))
                  (cl-return))))))
          (when matlab-ts-mode--ei-align-assign-alist
            (dolist (line-num line-nums)
              (push `(,line-num . ,assign-offset) matlab-ts-mode--ei-align-assign-alist))))

        (let ((diff (- assign-offset line-assign-offset)))
          (when (> diff 0)
            (let* ((loc (1- (string-match "=" ei-line)))
                   (new-pt-offset (let ((pt-offset (nth 1 ei-info)))
                                      (when pt-offset
                                        (if (<= loc pt-offset)
                                            (+ pt-offset diff)
                                          pt-offset)))))
              (setq ei-line (concat (substring ei-line 0 loc)
                                    (make-string diff ? )
                                    (substring ei-line loc)))
              (setq ei-info (list ei-line new-pt-offset (nth 2 ei-info) (nth 3 ei-info)))))))))
  ei-info)

(defun matlab-ts-mode--ei-trailing-comment-offset (ei-info)
  "Get trailing comment offset from first char current line?
See `matlab-ts-mode--ei-get-new-line' for EI-INFO contents.
To simplify implementation, we require that the first \"%\" character in
the line be the start of the trailing comment.  Thus,
  s = \"foo % bar\" % comment
is not identified as a trailing comment and
  s = \"foo bar\" %comment
is identified as a trailing comment."
  (when ei-info
    (save-excursion
      (beginning-of-line)
      (when (re-search-forward "%" (line-end-position) t)
        (let ((node (treesit-node-at (point))))
          (when (equal (treesit-node-type node) "comment")
            (let* ((new-line (nth 0 ei-info))
                   (offset (- (string-match-p "%" new-line) (string-match-p "[^ \t]" new-line))))
              (when (> offset 0)
                ;; have trailing comment at offset from first char in new-line
                offset))))))))

;; This is used to cache comment alignments for indent-region
(defvar-local matlab-ts-mode--ei-align-comment-alist nil)

(defun matlab-ts-mode--ei-align-trailing-comments (ei-info)
  "Align trailing comments in EI-INFO.
See `matlab-ts-mode--ei-get-new-line' for EI-INFO contents."
  (let* ((line-comment-offset (matlab-ts-mode--ei-trailing-comment-offset ei-info)))
    (when line-comment-offset
      (let* (comment-offset
             line-nums
             line-start-pt)

        (when (or (not matlab-ts-mode--ei-align-comment-alist)
                  (not (setq comment-offset (alist-get (line-number-at-pos)
                                                       matlab-ts-mode--ei-align-comment-alist))))
          (setq comment-offset line-comment-offset)
          (setq line-nums `(,(line-number-at-pos)))
          (save-excursion
            (beginning-of-line)
            (setq line-start-pt (point))

            ;; Look backwards and then forwards for lines with trailing comments
            (cl-loop
             for direction in '(-1 1) do
             (goto-char line-start-pt)
             (cl-loop
              while (not (if (= direction -1) (bobp) (eobp))) do
              (forward-line direction)
              (let* ((ei-l-info (matlab-ts-mode--ei-get-new-line))
                     (l-offset (matlab-ts-mode--ei-trailing-comment-offset ei-l-info)))
                (if l-offset
                    (progn
                      (push (line-number-at-pos) line-nums)
                      (when (> l-offset comment-offset)
                        (setq comment-offset l-offset)))
                  (cl-return))))))

          (when matlab-ts-mode--ei-align-comment-alist
            (dolist (line-num line-nums)
              (push `(,line-num . ,comment-offset) matlab-ts-mode--ei-align-comment-alist))))

        (let ((diff (- comment-offset line-comment-offset)))
          (when (> diff 0)
            (let* ((ei-line (nth 0 ei-info))
                   (loc (1- (string-match "%" ei-line)))
                   (new-pt-offset (let ((pt-offset (nth 1 ei-info)))
                                      (when pt-offset
                                        (if (<= loc pt-offset)
                                            (+ pt-offset diff)
                                          pt-offset)))))
              (setq ei-line (concat (substring ei-line 0 loc)
                                    (make-string diff ? )
                                    (substring ei-line loc)))
              (setq ei-info (list ei-line new-pt-offset (nth 2 ei-info) (nth 3 ei-info))))))))
    ei-info))

(defun matlab-ts-mode--ei-align (ei-info)
  "Align elements in EI-INFO.
See `matlab-ts-mode--ei-get-new-line' for EI-INFO contents."
  (let ((matrix-node (matlab-ts-mode--ei-point-in-m-matrix ei-info)))
    (if matrix-node
        (setq ei-info (matlab-ts-mode--ei-align-line-in-m-matrix matrix-node ei-info)))
    ;; else do single-line alignments
    (setq ei-info (matlab-ts-mode--ei-align-assignments ei-info))
    (setq ei-info (matlab-ts-mode--ei-align-trailing-comments ei-info)))
  ei-info)

(cl-defun matlab-ts-mode--ei-indent-elements-in-line (&optional start-node start-offset)
  "Indent current line by adjust spacing around elements.
Optional START-NODE and START-OFFSET are used to restore the point when
line is updated.  Returns t if line was updated."

  ;; If line was indented (ei-line is not same as current line), then update the buffer
  (let ((ei-info (matlab-ts-mode--ei-get-new-line start-node start-offset)))
    (when ei-info
      (when matlab-ts-mode--ei-align-enabled
        (setq ei-info (matlab-ts-mode--ei-align ei-info)))
      (let* ((ei-line (nth 0 ei-info))
             (pt-offset (nth 1 ei-info))
             (line-node-types (nth 2 ei-info))
             (curr-line (buffer-substring (line-beginning-position) (line-end-position)))
             (updated (not (string= curr-line ei-line))))

        (when updated
          (delete-region (line-beginning-position) (line-end-position))
          (insert ei-line)
          (when matlab-ts-mode--electric-indent-assert
            (matlab-ts-mode--ei-assert-match line-node-types))
          (when pt-offset
            (goto-char (+ (line-beginning-position) pt-offset))))
        ;; result
        updated))))

(provide 'matlab-ts-mode--ei)
;;; matlab-ts-mode--ei.el ends here
