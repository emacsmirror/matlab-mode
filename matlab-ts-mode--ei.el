;;; matlab-ts-mode--ei.el --- MATLAB electric indent -*- lexical-binding: t -*-

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
;;
;; Electric indent for matlab-ts-mode, see `matlab-ts-mode-electric-indent' custom variable.

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
- Canonicalize language element spacing
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

- Align properties and arguments
     Example                        |  Result
     -----------------------------  |  -----------------------------
     classdef c1                    |  classdef c1
         properties                 |      properties
             foo (1,3)              |          foo    (1,3)
             foobar (1,1)           |          foobar (1,1)
             p1 (1,1)               |          p1     (1,1)
         end                        |      end
     end                            |  end

- Untabify (convert TAB characters to spaces)"
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
      ))

(defvar matlab-ts-mode--ei-pad-op-re
  (rx bos (or "+" "-" "*" "/" ".*" "./" ".\\" "\\"
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

;; TODO optimize following by grouping together, also improve comments.  Perhaps write an optimizer
;; function so rows can remain.
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

    ;; Case: classdef property get/set
    ;; TopTester: electric_indent_classdef_prop_get_set.m
    (,(rx bos (or "get." "set.") eos) "."                                                        0)

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

    ;; Case arguments name=value syntax, the "=" node is converted to "n=v"
    ;; TopTester: electric_indent_name_value_args.m
    ("."                             ,(rx bos "n=v" eos)                                         0)
    (,(rx bos "n=v" eos)             "."                                                         0)

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

    ;; We shouldn't hit the ERROR node because matlab-ts-mode--ei-no-elements-to-indent
    ;; says to skip lines with ERROR nodes, but be safe in case an ERROR node spans
    ;; multiple lines without inner ERROR nodes.
    ;; TODO - double check this logic. Try creating a case that hits this by removing this line.
    (,(rx bos "ERROR" eos)            "."                                                        1)

    ))

(defun matlab-ts-mode--ei-fast-back-to-indentation ()
  "Fast back to indentation.
The `back-to-indentation' function uses the syntax table causes
slowdowns.  In MATLAB mode, the only whitespace characters we are
concerned with are SPC and TAB.  Thus, we can be fast.  Returns t if
there are non-whitespace characters on the line, nil otherwise."
  (forward-line 0)
  (if (re-search-forward "[^ \t]" (pos-eol) t)
      (progn
        (backward-char)
        t)
    (let ((inhibit-field-text-motion t)) (end-of-line))
    nil))

(defun matlab-ts-mode--ei-get-node-to-use (node)
  "Given NODE, adjust it for electric indent.

Returns (NODE . MODIFIED-NODE-TYPE) pair.

For example, when node parent is a string, we return the parent.  When
node is a \"+\" and parent is a unary_operator, we return MODIFIED-NODE-TYPE to
be unary-op even though the node type is \"+\"."

  (let* ((node-type (treesit-node-type node))
         (parent (treesit-node-parent node))
         (parent-type (or (treesit-node-type parent) "")))

    (cond
     ;; Use string and not the elements of the string
     ((string= parent-type "string")
      (setq node parent
            node-type parent-type))

     ((and (string= node-type "=")
           (string= parent-type "arguments"))
      ;; arguments name=value
      (setq node-type "n=v"))

     ;; convert property identifier to property-id node-type
     ((and (string= node-type "identifier")
           (or
            ;; propertyWithOutDot?
            (and (string= parent-type "property")
                 (equal (treesit-node-child parent 0) node))
            ;; property.nameWithDot?
            (and (string= parent-type "property_name")
                 (equal (treesit-node-child (treesit-node-parent parent) 0) parent))))
      (setq node-type "property-id"))

     ;; Unary operator sign, + or -, e.g. [0 -e] or g = - e
     ((and (string= parent-type "unary_operator")
           (equal (treesit-node-child parent 0) node))
      (setq node-type "unary-op"))

     ;; Super-class constructor call
     ;;  obj@myUtils.super;
     ((and (string= node-type "@")
           (string= parent-type "function_call"))
      (setq node-type "@-fcn-call"))

     ;; Property dimensions
     ;;   foo1 (1, :) {mustBeNumeric, mustBeReal} = [0, 0, 0];
     ((and (or (string= node-type "number") (string= node-type ":"))
           (or (string= parent-type "dimensions")
               (and (string= parent-type "spread_operator")
                    (string= (treesit-node-type (treesit-node-parent parent)) "dimensions"))))
      (setq node-type "prop-dim"))
     )

    (cons node node-type)))

(cl-defun matlab-ts-mode--ei-move-to-and-get-node ()
  "Move to and return node.
Will return nil if no next node before end-of-line.
Assumes point is at of current node or beginning of line.

Returns (NODE . MODIFIED-NODE-TYPE) where MODIFIED-NODE-TYPE
is used in `matlab-ts-mode--ei-spacing'"
  ;; Move point to first non-whitespace char
  (let ((eol (pos-eol)))
    (when (looking-at "[ \t]")
      (when (not (re-search-forward "[^ \t]" eol t))
        (cl-return-from matlab-ts-mode--ei-move-to-and-get-node))
      (backward-char))

    (let ((node (treesit-node-at (point))))

      ;; Consider [[1,2];[3,4]] when point is on semicolon, node will be the prior "]" because the
      ;; semicolon is an ignored node, so move forward to get to the "[" after the semicolon.
      (while (and (not (eobp))
                  (let ((node-start (treesit-node-start node)))
                    (and (>= node-start (pos-bol))
                         (< node-start (point)))))
        (forward-char)
        (setq node (treesit-node-at (point))))

      ;; Don't go past end-of-line point
      (when (or (equal "\n" (treesit-node-type node))
                (> (treesit-node-start node) eol)
                ;; When we get to EOL and in error context, node start will be on an earlier line
                ;;           x = [
                ;;  TAB>           1  ,   2  ;
                (< (treesit-node-start node) (pos-bol)))
        (goto-char eol)
        (setq node nil))

      (when node
        (matlab-ts-mode--ei-get-node-to-use node)))))

(defun matlab-ts-mode--ei-assert-match (orig-line orig-line-node-types)
  "Assert new line has same code meaning as ORIG-LINE.
This is done by validating that ORIG-LINE-NODE-TYPES matches the current
line node types.  We also validate line text matches ignoring
whitespace."
  (let* ((new-line (buffer-substring (pos-bol) (pos-eol)))
         (new-line-no-spaces (replace-regexp-in-string "[ \t]" "" new-line))
         (orig-line-no-spaces (replace-regexp-in-string "[ \t]" "" orig-line)))
    (when (not (string= new-line-no-spaces orig-line-no-spaces))
      (error "Assert: line-content-no-space-mismatch new: \"%s\" !EQ orig: \"%s\" at line %d in %s"
             new-line-no-spaces orig-line-no-spaces
             (line-number-at-pos (point)) (buffer-name))))

  (matlab-ts-mode--ei-fast-back-to-indentation)
  (let (curr-line-node-types)
    (cl-loop
     while (< (point) (pos-eol))
     do
     (let* ((pair (matlab-ts-mode--ei-move-to-and-get-node))
            (node (or (car pair)
                      (cl-return)))
            (node-type (cdr pair)))
       (setq curr-line-node-types
             (matlab-ts-mode--ei-update-line-node-types curr-line-node-types
                                                        node node-type))
       (let ((node-end (treesit-node-end node)))
         (if (< node-end (pos-eol))
             (goto-char node-end)
           (goto-char (pos-eol))))))

    (when (not (string= curr-line-node-types orig-line-node-types))
      (error "Assert: line-node-types mismatch new: \"%s\" !EQ orig: \"%s\" at line %d in %s"
             curr-line-node-types orig-line-node-types (line-number-at-pos (point)) (buffer-name)))))

(defun matlab-ts-mode--ei-concat-line (ei-line node extra-chars &optional n-spaces-to-append)
  "Return concat EI-LINE with NODE text.
NODE-END is the NODE end accounting for ignored nodes (semicolons).
EXTRA-CHARS are appended to EL-LINE.
N-SPACES-TO-APPEND is the number of spaces to append between nodes."

  (let* ((node-end (treesit-node-end node))
         (last-pt (if (< node-end (pos-eol)) node-end (pos-eol))))
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
                           (< last-pt (pos-eol)))
                  (save-excursion
                    (let ((inhibit-field-text-motion t)) (end-of-line))
                    (when (re-search-backward "[^ \t]" (pos-bol) t)
                      (forward-char)
                      (when (not (= (point) (pos-eol)))
                        (buffer-substring (point) (pos-eol))
                        ))))
              (when (> n-spaces-to-append 0)
                (make-string n-spaces-to-append ? ))))))

(defvar matlab-ts-mode--ei-error-query (treesit-query-compile 'matlab '((ERROR) @e)))
(defvar-local matlab-ts-mode--ei-errors-alist nil)

(defun matlab-ts-mode--ei-get-errors-alist ()
  "Return an alist of `(ERROR-LINENUM . t) elements.
If an error node spans multiple lines, we ignore it assuming
there's an inner error node.  This always returns non-nil, thus
enabling caching."
  (let ((capture-errors (treesit-query-capture (treesit-buffer-root-node)
                                               matlab-ts-mode--ei-error-query))
        (error-linenums `(-1 . nil)))
    (dolist (capture-error capture-errors)
      (let* ((error-node (cdr capture-error))
             (error-start-pt (treesit-node-start error-node))
             (error-end-pt (treesit-node-end error-node))
             (error-start-linenum (line-number-at-pos error-start-pt))
             (error-end-linenum (line-number-at-pos error-end-pt)))
        (when (= error-start-linenum error-end-linenum)
          (push `(,error-start-linenum . t) error-linenums))))
    error-linenums))

(defun matlab-ts-mode--ei-no-elements-to-indent ()
  "Return t if no elements in the current line to indent.
Assumes that current point is at `back-to-indentation'."
  (or
   ;; (1) Comment line? Nothing to indent in line if it's a comment line.
   (let ((first-node-type (or (treesit-node-type (treesit-node-at (point))) "")))
     (string-match-p (rx bos (or "line_continuation" "comment") eos) first-node-type))
   ;; (2) Syntax error *within* the line? If error node covers whole line, assume nodes in
   ;;     line are good, i.e. electric indent the line.
   (let ((error-linenums (or matlab-ts-mode--ei-errors-alist (matlab-ts-mode--ei-get-errors-alist)))
         (curr-linenum (line-number-at-pos (point))))
     (alist-get curr-linenum error-linenums))))

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

(defun matlab-ts-mode--ei-get-indent-level-spaces ()
  "Get indent-level spaces for current line expanding tabs."
  (let ((spaces (buffer-substring (pos-bol) (point))))
    (when (string-match "\t" spaces)
      (setq spaces (with-temp-buffer
                     (insert spaces)
                     (untabify (point-min) (point-max))
                     (buffer-string))))
    spaces))

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
    ;; Move to first non-whitespace character on the line
    (let ((have-non-empty-line (matlab-ts-mode--ei-fast-back-to-indentation)))
      (when (or (not have-non-empty-line)
                (matlab-ts-mode--ei-no-elements-to-indent))
        (cl-return-from matlab-ts-mode--ei-get-new-line)))

    ;; Compute ei-line, the electric indented line content
    (let* (pt-offset ;; used in restoring point
           (ei-line (matlab-ts-mode--ei-get-indent-level-spaces))
           (pair (matlab-ts-mode--ei-move-to-and-get-node))
           (node (or (car pair)
                     (cl-return-from matlab-ts-mode--ei-get-new-line)))
           (node-type (cdr pair))
           (first-node node)
           line-node-types
           next2-pair ;; used when we have: (NODE-RE (NEXT-NODE-RE NEXT2-NODE-RE) N-SPACES-BETWEEN)
           next2-n-spaces-between)

      (cl-loop
       while (and (< (point) (pos-eol))
                  (< (treesit-node-end node) (pos-eol)))
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
           (error "Assert: ei, unhandled node <\"%s\" %S> and next-node <\"%s\" %S>"
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
                            (min (treesit-node-end node) (pos-eol))
                            (pos-eol))))
          (setq ei-line (matlab-ts-mode--ei-concat-line ei-line node extra-chars))))

      (list ei-line pt-offset line-node-types first-node))))

(defvar-local matrix-ts-mode--ei-m-matrix-first-col-extra-alist nil)

(cl-defun matlab-ts-mode--ei-m-matrix-first-col-extra (matrix)
  "For MATRIX indent alignment, get first-col-extra."
  ;; For first-col-extra consider the following where matlab-ts-mode--array-indent-level is 2.
  ;; In this case first-col-extra will be 1.
  ;;   m = [
  ;;         1 2
  ;;         3 4
  ;;       ];
  (let* ((start-linenum (line-number-at-pos (treesit-node-start matrix)))
         (first-col-extra (or (alist-get start-linenum
                                         matrix-ts-mode--ei-m-matrix-first-col-extra-alist)
                              (save-excursion
                                (goto-char (treesit-node-start matrix))
                                (forward-char) ;; step over the "["
                                (let ((found-element nil))
                                  ;; found a matrix element?
                                  (while (and (not found-element)
                                              (re-search-forward "[^ \t]" (pos-eol) t))
                                    (backward-char)
                                    (let ((node (treesit-node-at (point))))
                                      (when (not (string-match-p (rx bos (or "comment"
                                                                             "line_continuation")
                                                                     eos)
                                                                 (treesit-node-type node)))
                                        (setq found-element t))
                                      (goto-char (min (pos-eol)
                                                      (treesit-node-end node)))))
                                  (let ((ans (if found-element
                                                 0
                                               (1- matlab-ts-mode--array-indent-level))))
                                    (when matrix-ts-mode--ei-m-matrix-first-col-extra-alist
                                      (push `(,start-linenum . ,ans)
                                            matrix-ts-mode--ei-m-matrix-first-col-extra-alist))
                                    ans))))))
    first-col-extra))

(defvar-local matlab-ts-mode--ei-m-matrix-col-widths-alist nil)

(defun matlab-ts-mode--ei-m-matrix-col-widths (matrix first-col-extra &optional first-col-only)
  "Get multi-line MATRIX column widths adding in FIRST-COL-EXTRA to first column.
If optional FIRST-COL-ONLY is non-nil, then return only the width of the
first column in MATRIX.
Returns alist where each element in the alist is (COLUMN-NUM . WIDTH)"

  (let* ((start-linenum (line-number-at-pos (treesit-node-start matrix)))
         (column-widths (alist-get start-linenum matlab-ts-mode--ei-m-matrix-col-widths-alist)))
    (when (not column-widths)
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

      (when matlab-ts-mode--ei-m-matrix-col-widths-alist
        (push `(,start-linenum . ,column-widths) matlab-ts-mode--ei-m-matrix-col-widths-alist)))

    column-widths))

(defun matlab-ts-mode--ei-get-m-matrix-row-in-line ()
  "Given point within a matrix assignment statement, return row node.
Note, nil may be returned when line is only a continuation, e.g.
   v = [1 2; ...
        ...
        3 4];
when on the 2nd continuation only line, nil is returned."
  (save-excursion
    (matlab-ts-mode--ei-fast-back-to-indentation)
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
           (goto-char (min (treesit-node-end node-at-pt) (pos-eol)))
           (when (not (re-search-forward "[^ \t]" (pos-eol) t))
             (setq found-ans t)))))
      row-node)))

;; Internal variable that shouldn't be altered. It's used to avoid infinite recursion.
(defvar matlab-ts-mode--ei-align-enabled t)

;; This is used to cache matrix alignments for indent-region
;; It will be non-nil when called from indent-region.
(defvar matlab-ts-mode--ei-align-matrix-alist nil)

(defun matlab-ts-mode--ei-indent-matrix-in-tmp-buf (assign-node)
  "Insert ASSIGN-NODE in to current tmp-buf and indent.
Point is left at beginning of line containing the ASSIGN-NODE text.
Returns the line number after the ASSIGN-NODE in the tmp-buf."
  (let (assign-str
        n-levels
        end-linenum)
    (with-current-buffer (treesit-node-buffer assign-node)
      (let* ((assign-start-pos (save-excursion (goto-char (treesit-node-start assign-node))
                                               (pos-bol)))
             (assign-end-pos (save-excursion (goto-char (treesit-node-end assign-node))
                                             (pos-eol)))
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

    (setq end-linenum (line-number-at-pos))

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
      (forward-line n-levels))
    end-linenum))

;; TODO - investigate performance improvements for m-matrix line alignment.
;; 1. Should we improve performance by leveraging prior line when TABing lines?
;; 2. When indent-region is active, can we speedup calculation of column widths?

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
         (matrix-alist matlab-ts-mode--ei-align-matrix-alist)
         end-linenum)
    (with-temp-buffer
      (setq end-linenum (matlab-ts-mode--ei-indent-matrix-in-tmp-buf assign-node))

      (let* ((matrix-node (treesit-node-parent (treesit-search-subtree
                                                (treesit-buffer-root-node) (rx bos "[" eos) nil t)))
             (first-col-extra (matlab-ts-mode--ei-m-matrix-first-col-extra matrix-node))
             (column-widths (matlab-ts-mode--ei-m-matrix-col-widths matrix-node first-col-extra)))

        ;; Move to the line of interest when we called from matlab-ts-mode--treesit-indent,
        ;; otherwise calculate all matrix rows for indent-region.
        (when (and (not matrix-alist)
                   (> tmp-buf-ei-linenum 1))
          (forward-line (1- tmp-buf-ei-linenum)))

        (while (< (line-number-at-pos) end-linenum) ;; Adjust column widths
          (matlab-ts-mode--ei-fast-back-to-indentation)
          (let* ((row-node (matlab-ts-mode--ei-get-m-matrix-row-in-line))
                 (ei-line (buffer-substring (pos-bol) (pos-eol)))
                 (indent-offset (string-match-p "[^ \t]+" ei-line)) ;; nil if at blank line in matrix
                 n-spaces)
            (when (and row-node indent-offset)
              (let* ((col-num (length column-widths))
                     (indent-start-pt (point))
                     ;; line content does not have leading indent-level spaces
                     (content (buffer-substring indent-start-pt (pos-eol)))
                     (pt-offset (nth 1 ei-info))
                     (matrix-offset (save-excursion
                                      (goto-char (treesit-node-start matrix-node))
                                      (1+ (- (point) (pos-bol))))))

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
    (while (re-search-forward "[^ \t]" (pos-eol) t)
      (backward-char)
      (let ((node (treesit-node-at (point))))
        (when (not (string-match-p (rx bos (or "," ";" "comment" "line_continuation") eos)
                                   (treesit-node-type node)))
          (cl-return-from matlab-ts-mode--ei-matrix-ends-on-line))
        (goto-char (treesit-node-end node)))))
  t)

(defvar-local matlab-ts-mode--ei-is-m-matrix-alist nil) ;; cache

(cl-defun matlab-ts-mode--ei-is-m-matrix (matrix)
  "Is MATRIX node a multi-line matrix?
We define a a multi-line matrix has one row per line and more than one
column."
  (let* ((start-linenum (line-number-at-pos (treesit-node-start matrix)))
         (cache-value (alist-get start-linenum matlab-ts-mode--ei-is-m-matrix-alist)))

    (when cache-value ;; 0 or 1
      (cl-return-from matlab-ts-mode--ei-is-m-matrix (= cache-value 1)))

    (let ((end-line (line-number-at-pos (treesit-node-end matrix)))
          (n-rows 0)
          n-cols)

      (when (and (> end-line start-linenum) ;; multi-line matrix?
                 (matlab-ts-mode--ei-matrix-ends-on-line matrix)
                 (not (treesit-search-subtree matrix (rx bos "ERROR" eos) nil t)))
        (dolist (child (treesit-node-children matrix))
          (let ((child-type (treesit-node-type child)))
            (cond
             ((string= child-type "row")
              (let ((row-start-linenum (line-number-at-pos (treesit-node-start child))))
                ;; Return nil if row not on one line
                (when (not (= row-start-linenum (line-number-at-pos (treesit-node-end child))))
                  (cl-return-from matlab-ts-mode--ei-is-m-matrix))
                ;; Return nil if more than one row one the line
                (let ((next-node (treesit-node-next-sibling child)))
                  (when (and (string= (treesit-node-type next-node) "row")
                             (= row-start-linenum (line-number-at-pos
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

      ;; Multi-line matrix (m-matrix) with each row is on its own line?
      (let ((ans (and (> n-rows 1) (>= n-cols 1))))
        (when matlab-ts-mode--ei-is-m-matrix-alist
          ;; Use 1 or 0 so we can differentiate between nil and not a multi-line matrix
          (push `(,start-linenum . ,(if ans 1 0)) matlab-ts-mode--ei-is-m-matrix-alist))
        ans))))

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
        (forward-line 0)
        (when (re-search-forward "=" (pos-eol) t)
          (backward-char)
          (let ((eq-node (treesit-node-at (point))))
            ;; First "=" must be an assignment (assumptions elsewhere require this).
            (when (and (equal (treesit-node-type eq-node) "=")
                       (equal (treesit-node-type (treesit-node-parent eq-node)) "assignment"))
              (cond
               ;; Single-line assignment? Example: v1 = [1, 2];
               ((eq type 'single-line)
                (when (<= (treesit-node-end assign-node) (pos-eol))
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
        (matlab-ts-mode--ei-fast-back-to-indentation)
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
            (forward-line 0)
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

(defun matlab-ts-mode--ei-get-prop-node (ei-info)
  "Return property or argument node for first node in EI-INFO.
Returns nil if not a property or argument node."
  (let* ((first-node-in-line (nth 3 ei-info))
         (parent (when first-node-in-line (treesit-node-parent first-node-in-line)))
         (prop-node (pcase (treesit-node-type parent)
                      ("property"
                       first-node-in-line)
                      ("property_name"
                       parent))))
    ;; properties / arguments can span multiple lines, so skip these
    (when (= (line-number-at-pos (treesit-node-start prop-node))
             (line-number-at-pos (treesit-node-end prop-node)))
      prop-node)))

(defun matlab-ts-mode--ei-prop-length (ei-info)
  "Get the property length from the electric indented line in EI-INFO."
  ;; We need to use the ei-line and not the current content of the buffer because the ei-line could
  ;; have been adjusted. Example:
  ;;    arguments
  ;;       nameValueArgs .  foo ( 1,1)
  ;;    end
  ;; becomes "nameValueArgs.foo (1,1)" in EI-LINE.
  (let ((ei-line (nth 0 ei-info)))
    (when (not (string-match "[^ \t]+" ei-line))
      (error "Assert: no property in ei-line %s" ei-line))
    (- (match-end 0) (match-beginning 0))))

;; This is used to cache aligned properties/arguments for indent-region, and contains
;; '(linenum . prop-length) entries.
(defvar-local matlab-ts-mode--ei-align-prop-alist nil)

(defun matlab-ts-mode--ei-align-properties (ei-info)
  "Align properties and arguments in EI-INFO.
See `matlab-ts-mode--ei-get-new-line' for EI-INFO contents."
  (when (matlab-ts-mode--ei-get-prop-node ei-info)
    (let ((ei-info-p-length (matlab-ts-mode--ei-prop-length ei-info))
          (p-length (when matlab-ts-mode--ei-align-prop-alist
                      (alist-get (line-number-at-pos) matlab-ts-mode--ei-align-prop-alist))))
      (when (not p-length)
        (save-excursion
          (forward-line 0)
          (let* ((line-nums `(,(line-number-at-pos)))
                 (line-start-pt (point)))

            (setq p-length ei-info-p-length)

            ;; Look backwards and then forwards for properties/arguments
            (cl-loop
             for direction in '(-1 1) do

             (goto-char line-start-pt)
             (cl-loop
              while (not (if (= direction -1) (bobp) (eobp))) do
              (forward-line direction)
              (let* ((l-ei-info (matlab-ts-mode--ei-get-new-line)))
                (if (matlab-ts-mode--ei-get-prop-node l-ei-info)
                    (let ((l-p-length (matlab-ts-mode--ei-prop-length l-ei-info)))
                      (when matlab-ts-mode--ei-align-prop-alist
                        (push (line-number-at-pos) line-nums))
                      (when (> l-p-length p-length)
                        (setq p-length l-p-length)))
                  (cl-return)))))

            (when matlab-ts-mode--ei-align-prop-alist
              (dolist (line-num line-nums)
                (push `(,line-num . ,p-length) matlab-ts-mode--ei-align-prop-alist))))))

      (when (not (= p-length ei-info-p-length))
        (let* ((diff (- p-length ei-info-p-length))
               (ei-line (nth 0 ei-info))
               (p-end (when (string-match "\\([^ \t]+\\)[ \t]+[^ \t;]" ei-line)
                        (match-end 1))))
          (when p-end
            (let ((new-pt-offset (let ((pt-offset (nth 1 ei-info)))
                                   (when pt-offset
                                     (if (<= p-end pt-offset)
                                         (+ pt-offset diff)
                                       pt-offset)))))
              (setq ei-line (concat (substring ei-line 0 p-end)
                                    (make-string diff ? )
                                    (substring ei-line p-end)))
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
      (forward-line 0)
      (when (re-search-forward "%" (pos-eol) t)
        (let ((node (treesit-node-at (point))))
          (when (and (equal (treesit-node-type node) "comment")
                     ;; And not for a control flow or definition statement. For example,
                     ;;    if a % comment1                  <= we shouldn't align this
                     ;;       b = [1, 2, 3]; % comment2
                     ;;       c = 1;         % comment3
                     (progn
                       (forward-line 0)
                       (while (and (not (bobp))
                                   (save-excursion
                                     (backward-char)
                                     (when (re-search-backward "[^ \t]" (pos-bol) t)
                                       (equal (treesit-node-type (treesit-node-at (point)))
                                              "line_continuation"))))
                         (forward-line -1))
                       (matlab-ts-mode--ei-fast-back-to-indentation)
                       (let ((node (treesit-node-at (point))))
                         (not (string-match-p matlab-ts-mode--ei-keywords-re
                                              (treesit-node-type node))))))

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
            (forward-line 0)
            (setq line-start-pt (point))

            ;; Look backwards and then forwards for lines with trailing comments
            (cl-loop
             for direction in '(-1 1) do
             (goto-char line-start-pt)
             (cl-loop
              while (not (if (= direction -1) (bobp) (eobp))) do
              (forward-line direction)
              (let ((ei-l-info (matlab-ts-mode--ei-get-new-line))
                    l-offset)
                (setq ei-l-info (matlab-ts-mode--ei-align-assignments ei-l-info))
                (setq ei-l-info (matlab-ts-mode--ei-align-properties ei-l-info))
                (setq l-offset (matlab-ts-mode--ei-trailing-comment-offset ei-l-info))
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
    (setq ei-info (matlab-ts-mode--ei-align-properties ei-info))
    (setq ei-info (matlab-ts-mode--ei-align-trailing-comments ei-info)))
  ei-info)

(defun matlab-ts-mode--ei-get-start-info ()
  "Get start node and start offset in line prior to electric indent.
Returns (cons start-node start-offset) where
- start-node is non-nil if the point is at a node and start-node will
  be the modified node from `matlab-ts-mode--ei-get-node-to-use'
- start-offset is the offset of the point from the beginning of start-node.
Example where point is on the \"d\" in width:
  area = width * length;
           ^
start-node is the identifier node for width and start-offset is 2."
  (let (start-node
        start-offset
        (at-eol (looking-at "[ \t]*$")))

    ;; The node when at-eol is not on the current line.
    (when (not at-eol)
      (let ((node (treesit-node-at (point))))
        ;; Consider:  A = [B   C];
        ;;                   ^
        ;; node is the invisible "," and moving to start gives us node for C
        (save-excursion
          (when (< (point) (treesit-node-start node))
            (if (re-search-forward "[^ \t]" (pos-eol) t)
                (progn
                  (backward-char)
                  (setq node (treesit-node-at (point))))
              (if (re-search-backward "[^ \t]" (pos-bol) t)
                  (setq node (treesit-node-at (point)))
                (setq node nil))))

          (when (and node
                     (>= (point) (treesit-node-start node))
                     (<= (point) (treesit-node-end node)))
            (setq start-node (car (matlab-ts-mode--ei-get-node-to-use node)))
            (setq start-offset (- (point) (treesit-node-start node)))))))

    (cons start-node start-offset)))

(defun matlab-ts-mode--ei-workaround-143 (beg end &optional line-pt)
  "Workaround https://github.com/acristoffers/tree-sitter-matlab/issues/143.
Between BEG and END points, insert a space between numbers and dot
operators.  For example,

  123./  =>  123 ./
  123.*  =>  123 .*
  123.\\  =>  123 .\\

When optional LINE-PT point is specified, it must be between BEG and END
inclusive and the adjusted LINE-PT will be adjusted to stay on the same
semantic element.  For example if BEG and END are the line beginning and
end positions of the following assignment line, LINE-PT is adjusted as
shown.

      x = 123./1 + 567
                   ^         <= LINE-PT
TAB>  x = 123 ./1 + 567
                    ^        <= LINE-PT adjusted to here"
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "[0-9]\\.[/\\*\\\\]" end t)
      (backward-char 2) ;; on the "."
      (let ((node (treesit-node-at (point))))
        (when (equal (treesit-node-type node) "number")
          (when (and line-pt
                     (<= (point) line-pt))
            (setq line-pt (1+ line-pt)))
          (insert " ")
          (setq end (1+ end))))
      (forward-char 2)))
  (when line-pt
    (goto-char line-pt)))

(cl-defun matlab-ts-mode--ei-indent-elements-in-line (&optional is-indent-region start-pt-offset)
  "Indent current line by adjusting spacing around elements.

When IS-INDENT-REGION is t, we return (list NEW-LINE UPDATED
NEW-START-PT-OFFSET).  Optional START-PT-OFFSET is used only when
IS-INDENT-REGION is t and when START-PT-OFFSET is non-nil NEW-START-PT-OFFSET
is non-nil.  This is used to update the point location for
`matlab-ts-mode-prog-fill-reindent-defun'.

When IS-INDENT-REGION is nil, we update the line and restore the point
to it's logical location when the line is updated."

  ;; If line was indented (nth 0 ei-info) is not same as current line, then update the buffer
  (let* ((start-pair (when (or (not is-indent-region)
                               start-pt-offset)
                       (matlab-ts-mode--ei-get-start-info)))
         (start-node (car start-pair)) ;; may be nil
         (start-offset (cdr start-pair))
         (at-eol (and start-offset (looking-at "[ \t]*$")))
         (orig-line (buffer-substring (pos-bol) (pos-eol)))
         (ei-info (matlab-ts-mode--ei-get-new-line start-node start-offset)))
    (if ei-info
        (progn
          (when matlab-ts-mode--ei-align-enabled
            (setq ei-info (matlab-ts-mode--ei-align ei-info)))
          (let* ((ei-line (or (nth 0 ei-info) orig-line))
                 (pt-offset (nth 1 ei-info)) ;; non-nil if start-offset is non-nil
                 (line-node-types (nth 2 ei-info))
                 (updated (and ei-info
                               (not (string= orig-line ei-line)))))

            (when (and updated
                       pt-offset
                       at-eol)
              (setq pt-offset (length ei-line)))

            (when (and updated
                       matlab-ts-mode--electric-indent-assert)
              (matlab-ts-mode--ei-assert-match orig-line line-node-types))

            (if is-indent-region
                (list ei-line updated pt-offset) ;; result
              ;; Else updated the line if needed (TAB on a line to electric indents it).
              (when updated
                (delete-region (pos-bol) (pos-eol))
                (insert ei-line)
                (when pt-offset
                  (goto-char (+ (pos-bol) pt-offset))))
              nil ;; return nil for TAB indent
              )))
      ;; else nothing updated
      (when is-indent-region
        (list orig-line 0 nil))
      )))

(defun matlab-ts-mode--ei-indent-region (beg end)
  "Indent BEG END region by adjusting spacing around elements.
If BEG is not at start of line, it is moved to start of the line.
If END is not at end of line, it is moved to end of the line.
This expansion of the region is done to simplify electric indent."

  ;; We need to run electric indent before treesit-indent-region. Consider
  ;;    l2 = @(x)((ischar(x) || isstring(x) || isnumeric(x)) && ...
  ;;                 ~strcmpi(x, 'fubar'));
  ;; If we indent-region first, we'll get
  ;;    l2 = @(x)((ischar(x) || isstring(x) || isnumeric(x)) && ...
  ;;              ~strcmpi(x, 'fubar'));
  ;; then when we adjust spacing, we'll have the following where the 2nd line is not
  ;; indented correctly.
  ;;    l2 = @(x) ((ischar(x) || isstring(x) || isnumeric(x)) && ...
  ;;              ~strcmpi(x, 'fubar'));
  (let* ((curr-linenum (line-number-at-pos beg))
         (end-linenum (save-excursion
                        (goto-char end)
                        (let ((inhibit-field-text-motion t)) (end-of-line))
                        (let ((linenum (line-number-at-pos)))
                          (when (= (point) (pos-bol))
                            (setq linenum (1- linenum)))
                          linenum)))
         (start-pt (point))
         (start-pt-linenum (line-number-at-pos start-pt))
         (start-pt-offset (- start-pt (save-excursion
                                        (goto-char start-pt)
                                        ;; offset from beginning of start-pt-linenum
                                        (pos-bol))))
         new-content-buf)

    (matlab-ts-mode--ei-workaround-143 beg end) ;; may insert spaces on lines in BEG END region

    (unwind-protect
        (progn
          ;; Add an invalid entry to each of the following associative lists. This entry is used
          ;; as a marker to activate caching. Each entry in the lists is a cons cell `(LINENUM
          ;; . INFO) where -1 is not a valid line number.
          (setq-local matlab-ts-mode--ei-align-assign-alist '((-1 . 0))
                      matlab-ts-mode--ei-align-prop-alist '((-1 . 0))
                      matlab-ts-mode--ei-align-comment-alist '((-1 . 0))
                      matlab-ts-mode--ei-m-matrix-col-widths-alist '((-1 . 0))
                      matrix-ts-mode--ei-m-matrix-first-col-extra-alist '((-1 . 0))
                      matlab-ts-mode--ei-is-m-matrix-alist '((-1 . 0))
                      matlab-ts-mode--ei-align-matrix-alist '((-1 . ""))
                      matlab-ts-mode--ei-errors-alist (matlab-ts-mode--ei-get-errors-alist))

          (save-excursion

            ;; Move END point to end of line.
            ;; To do this, we use end-linenum, because workaround-143 could have moved END.
            (goto-char (point-min))
            (when (> end-linenum 1)
              (forward-line (1- end-linenum)))
            (let ((inhibit-field-text-motion t)) (end-of-line))
            (setq end (point))

            ;; Move BEG to beginning of line and leave point there.
            (goto-char beg)
            (forward-line 0)
            (setq beg (point))

            (setq new-content-buf (get-buffer-create
                                   (generate-new-buffer-name " *temp-matlab-indent-region*")))
            (let (region-updated)

              (while (<= curr-linenum end-linenum)
                (forward-line 0)

                (let* ((tuple (matlab-ts-mode--ei-indent-elements-in-line
                               'indent-region
                               (when (= curr-linenum start-pt-linenum) start-pt-offset)))
                       (new-line (nth 0 tuple))
                       (line-updated (nth 1 tuple))
                       (new-start-pt-offset (nth 2 tuple)))

                  (when new-start-pt-offset
                    (setq start-pt-offset new-start-pt-offset))

                  (with-current-buffer new-content-buf
                    (insert new-line)
                    (when (< curr-linenum end-linenum)
                      (insert "\n")))

                  (when line-updated
                    (setq region-updated t)))

                (forward-line)
                (setq curr-linenum (1+ curr-linenum)))

              (when region-updated
                (save-excursion
                  (goto-char beg)
                  (delete-region beg end)
                  (insert (with-current-buffer new-content-buf
                            (buffer-string)))

                  ;; Restore end point accounting for whitespace adjustments in the lines
                  (goto-char (point-min))
                  (forward-line end-linenum)
                  (setq end (point))))))

          ;; Update point to keep it on the starting semantic element
          (goto-char (point-min))
          (forward-line (1- start-pt-linenum))
          (forward-char start-pt-offset)

          ;; Now indent-region (this uses the matlab-ts-mode--ei-*-alist caches)
          (treesit-indent-region beg end))

      ;; unwind-protect cleanup
      (setq-local matlab-ts-mode--ei-align-assign-alist nil
                  matlab-ts-mode--ei-align-prop-alist nil
                  matlab-ts-mode--ei-align-comment-alist nil
                  matlab-ts-mode--ei-m-matrix-col-widths-alist nil
                  matrix-ts-mode--ei-m-matrix-first-col-extra-alist nil
                  matlab-ts-mode--ei-is-m-matrix-alist nil
                  matlab-ts-mode--ei-align-matrix-alist nil
                  matlab-ts-mode--ei-errors-alist nil)
      (kill-buffer new-content-buf))))

(provide 'matlab-ts-mode--ei)
;;; matlab-ts-mode--ei.el ends here

;; LocalWords:  SPDX gmail treesit defcustom bos eos isstring defun eol eobp setq curr cdr xr progn
;; LocalWords:  listp alist dolist setf tmp buf utils linenum nums bobp pcase Untabify untabify SPC
;; LocalWords:  linenums reindent bol fubar
