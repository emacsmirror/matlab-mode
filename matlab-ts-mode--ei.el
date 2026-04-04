;;; matlab-ts-mode--ei.el --- MATLAB electric indent -*- lexical-binding: t -*-

;; Version: 8.1.2
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
;; Electric indent: standardize language element spacing, align consecutive statements, columns,
;; align trailing comments, add missing commas to arrays, aligns multi-line matrices columns, and
;; remove tabs.

;;; Code:

(require 'treesit)

(defvar matlab-ts-mode--array-indent-level)
(defvar matlab-ts-mode--indent-level)
(declare-function matlab-ts-mode "matlab-ts-mode.el")

(defgroup matlab-ts nil
  "MATLAB(R) tree-sitter mode."
  :prefix "matlab-ts-mode-"
  :group 'languages)

(defvar matlab-ts-mode--electric-indent t) ;; This shouldn't be disabled - it's here for testing.

(defvar matlab-ts-mode--electric-indent-verbose nil)
(defvar matlab-ts-mode--indent-assert) ;; from matlab-ts-mode.el

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
  (rx bos (or "+" "-" "*" "/" ".*" "./" ".\\" "\\"
              "==" "~=" ">" ">=" "<" "<="
              "&" "|" "&&" "||"
              "="
              "^" ".^"
              "'" ".'"
              ":")
      eos))

(defvar matlab-ts-mode--ei-val-re (rx bos (or "identifier" "number") eos))

;; TODO optimize following by grouping together, also improve comments.  Perhaps write an optimizer
;; function so rows can remain.
(defvar matlab-ts-mode--ei-spacing
  ;; In a given line, we walk across the nodes adjusting spaces between NODE and NEXT-NODE to
  ;; have N-SPACES-BETWEEN them.
  ;;
  ;; NODE-RE                          NEXT-NODE-RE                                 N-SPACES-BETWEEN
  `(

    ("."                              ,(rx bos (or "dim-(" "comment" "line_continuation") eos)   1)

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

    ;; Case 10 .^ a   (must have a space between a number an dot to avoid changing node type)
    (,(rx bos "number" eos)           ,(rx bos ".^" eos)                                         1)

    ;; Case: power and transpose: a^b a.^b
    (,matlab-ts-mode--ei-val-re       (,(rx bos (or "^" ".^") eos) . ,matlab-ts-mode--ei-val-re) 0)

    ;; Case: anything followed by ",", etc.: [a, b]
    ("."                              ,(rx bos (or "," ";" ".") eos)                             0)

    ;; Case lambda: @(x), metaclass operator, ?
    (,(rx bos (or "@" "?") eos)       "."                                                        0)

    ;; Case: anything after a ".", e.g. foo.bar, s.(fieldName)
    (,(rx bos "." eos)                "."                                                        0)

    (,(rx bos (or "," ";" "command_argument" "command_name" "enum-id") eos)  "."                 1)

    ;; Case: open bracket etc., not (~var), unary operator (+123, -123)
    ;;       Example: [123
    ;;                 ^
    (,(rx bos (or "[" "{" "(" "~" "unary-op") eos)  "."                                          0)

    ;; Case: close bracket, etc.
    (,(rx bos "]" eos)                ,(rx bos (or "," ";") eos)                                 0)
    (,(rx bos "]" eos)                ,(rx bos "[" eos)                                          1)
    ("."                              ,(rx bos (or "]" ")" "}" "lambda-)") eos)                  0)

    ;; Case: ") identifier" as in: propName (1, 1) double
    ;;       arguments:  g (1,1) {mustBeNumeric, mustBeReal}
    (,(rx bos "dim-)" eos)            "."                                                        1)
    (,(rx bos "prop-dim" eos)         ,(rx bos "dim-)" eos)                                      0)

    ;; Case: @(x) ((ischar(x) || isstring(x)));
    ;;          ^
    (,(rx bos "lambda-)" eos)         "."                                                        1)

    ;; Case: property identifier (the prop or class): propName (1,1) double
    (,(rx bos (or "prop-id" "prop-class-id") eos)   "."                                          1)

    ;; Case: padded operators, e.g.: a || b     c * d
    (,matlab-ts-mode--ei-pad-op-re    "."                                                        1)
    ("."                              ,matlab-ts-mode--ei-pad-op-re                              1)

    ;; Case: string followed by anything, e.g. ["string1" foo(1)]
    (,(rx bos "string" eos)           "."                                                        1)

    ;; Case: anything before string, e.g. [foo(1) "string1"]
    ("."                              ,(rx bos "string" eos)                                     1)

    ;; Case: c3 = {b [c '%']};
    ("."                              ,(rx bos "[" eos)                                          1)

    ;; Case: foo(1) or foo.bar
    ;; Case: events(thing), enumeration(thing), methods(thing)
    (,(rx bos (or (seq (1+ (in "a-z")) "-fcn") "identifier") eos)  ,(rx bos (or "(" "{") eos)    0)
    (,(rx bos "identifier" eos)       "."                                                        1)
    (,(rx "-fcn" eos)                 "."                                                        1)

    ;; Case: number in matrix: [123 456]
    (,(rx bos "number" eos)           "."                                                        1)

    ;; Case: subclass
    (,(rx bos "<" eos)                "."                                                        1)

    ;; Case: keywords, e.g. if condition
    ;;                         ^
    (,matlab-ts-mode--ei-keywords-re  "."                                                        1)

    ;; Case: c = {['freq' '%'] num2str(2)};
    (,(rx bos "]" eos)                "."                                                        1)

    ;; Case: c4{1} = [1 2; 3 4];
    ;;       v4 = [c4{1}(1,1), c4{1}(1,1)];
    ;;       {s.('field1')(3)}
    (,(rx bos (or "}" ")") eos)       ,(rx bos (or "(" "{") eos)                                 0)

    ;; Case: ")": m3 = uint8([ones(20,1); 2*ones(8,1)]);
    ;;                                 ^  ^
    ;; Case: c3 = {{[17.50 0] [17.50 0]} {[120 0] [120 20]}};
    ;;                                 ^ ^
    (,(rx bos (or ")" "}") eos)       "."                                                        1)

    ;; We shouldn't hit the ERROR node because matlab-ts-mode--ei-no-elements-to-indent
    ;; says to skip lines with ERROR nodes, but be safe in case an ERROR node spans
    ;; multiple lines without inner ERROR nodes.
    (,(rx bos "ERROR" eos)            "."                                                        1)

    ))

;; matlab-ts-mode--ei-spacing-fast
;;    This is used to avoid scanning the matlab-ts-mode--ei-spacing rules.  This fast lookup
;;    resolves the vast majority of pairs (especially the number-number case dominant in matrix
;;    data) in O(1) via hash, falling back to the original linear scan only for rare unmatched
;;    pairs.
(defvar matlab-ts-mode--ei-spacing-fast
  (let ((ht (make-hash-table :test 'equal))
        ;; all-types are the MODIFIED-NODE-TYPE values returned by
        ;; `matlab-ts-mode--ei-line-nodes-in-region'. This will need to be updated
        ;; as `matlab-ts-mode--ei-line-nodes-in-region' evolves. We assert this below.
        (all-types '(
                     ;; Non-modified types that match capture nodes
                     "identifier" "number" "string" "comment" "line_continuation"
                     "command_argument" "command_name"
                     "," ";" "." "(" ")" "[" "]" "{" "}"
                     "+" "-" "*" "/" "\\" "^" ":" "="
                     ".*" "./" ".\\" ".^" ".?" ".'"
                     "<" ">" "<=" ">=" "==" "~="
                     "&" "|" "&&" "||"
                     "~" "@" "?" "'"
                     ;; Non-modified type keywords
                     "arguments" "break" "case" "catch" "classdef" "continue"
                     "else" "elseif" "end" "enumeration" "events" "for" "function"
                     "get." "global" "if" "methods" "otherwise" "parfor" "persistent"
                     "properties" "return" "set." "spmd" "switch" "try" "while"
                     ;; Modified types
                     "prop-id" "prop-class-id" "enum-id" "prop-dim"
                     "unary-op" "@-fcn-call" "dim-(" "dim-)" "lambda-)"
                     ;; Keyword-command modified types
                     "events-fcn" "enumeration-fcn" "methods-fcn" "arguments-fcn"
                     ;; Safety
                     "ERROR")
                   )
        (matched-node-re-ht (make-hash-table :test 'equal)))

    (dolist (tuple matlab-ts-mode--ei-spacing)
      (let* ((node-re   (nth 0 tuple))
             (next-spec (nth 1 tuple))
             (is-3node  (and (listp next-spec) (consp next-spec)))
             (next-node-re (if is-3node (car next-spec) next-spec))
             (n-spaces  (nth 2 tuple))

             matched-node-re)

        (if is-3node
            ;; Skip the 3-node rule — it is handled specially in the lookup function
            (setq matched-node-re node-re)
          (dolist (node-type all-types)
            (when (string-match-p node-re node-type)
              (setq matched-node-re node-re)
              (let (found-next-node-re)
                (dolist (next-node-type all-types)
                  (when (string-match-p next-node-re next-node-type)
                    (setq found-next-node-re t)
                    (let ((key (cons node-type next-node-type)))
                      ;; First match rule wins
                      (unless (gethash key ht)
                        (puthash key n-spaces ht)))))
                (cl-assert found-next-node-re)))))
        (cl-assert matched-node-re)
        (puthash matched-node-re t matched-node-re-ht)))
    ;; Assert we've covered all tuples in the ht
    (dolist (tuple matlab-ts-mode--ei-spacing)
      (let ((node-re (nth 0 tuple)))
        (cl-assert (gethash node-re matched-node-re-ht))))
    ;; Result
    ht)
  "Hash (node-type . next-node-type) -> n-spaces for fast exact-pair dispatch.
Enumerates all concrete (node-type . next-node-type) pairs implied by
the spacing rules.  Rules are processed in order so the first match wins.")

(defun matlab-ts-mode--assert-msg (msg)
  "Call error with MSG for code that shouldn't be hit."
  (error "Assert: %s" msg))

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
     ;; Case: Use string and not the elements of the string
     ((string= parent-type "string")
      (setq node parent
            node-type parent-type))

     ;; Case: prop-id, prop-class-id, enum-id
     ((string= node-type "identifier")
      (cond ((string= parent-type "property") ;; propertyWithOutDot?
             (if (equal (treesit-node-child parent 0) node)
                 (setq node-type "prop-id")
               (setq node-type "prop-class-id")))
            ((string= parent-type "property_name") ;; property.nameWithDot?
             (if (equal (treesit-node-child (treesit-node-parent parent) 0) parent)
                 (setq node-type "prop-id")
               (setq node-type "prop-class-id")))
            ((string= parent-type "enum")
             (setq node-type "enum-id"))
            ))

     ;; Case: unary operator sign, + or -, e.g. [0 -e] or g = - e
     ((and (string= parent-type "unary_operator")
           (equal (treesit-node-child parent 0) node))
      (setq node-type "unary-op"))

     ;; Case: super-class constructor call
     ;;  obj@myUtils.super;
     ((and (string= node-type "@")
           (string= parent-type "function_call"))
      (setq node-type "@-fcn-call"))

     ;; Case: property dimensions
     ;;   foo1 (1, :) {mustBeNumeric, mustBeReal} = [0, 0, 0];
     ((and (or (string= node-type "number") (string= node-type ":"))
           (or (string= parent-type "dimensions")
               (and (string= parent-type "spread_operator")
                    (string= (treesit-node-type (treesit-node-parent parent)) "dimensions"))))
      (setq node-type "prop-dim"))

     ;; Case: events, enumeration, methods
     ((and (string-match-p (rx bos (or "events" "enumeration" "methods" "arguments") eos) node-type)
           (string= parent-type "identifier"))
      ;; TopTester: electric_indent_inspect_keyword_commands.m
      ;; TopTester: electric_indent_inspect_keyword_commands2.m
      (setq node-type (concat node-type "-fcn")))

     ;; Case: lambda:     @(x) ((ischar(x) || isstring(x)))
     ;;                      ^
     ;;       properties: foo1 (1, :) {mustBeNumeric, mustBeReal} = [0, 0, 0];
     ;;                             ^
     ((string= node-type ")")
      (if (string= parent-type "lambda")
          (setq node-type "lambda-)")
        (if (string= parent-type "dimensions")
            (setq node-type "dim-)"))
        ))

     ;; Case: arguments fcn keyword: arguments (1, :) {mustBeNumeric}
     ;;                                        ^
     ((and (string= node-type "(")
           (string= parent-type "dimensions"))
      (setq node-type "dim-(")))

    (cons node node-type)))

;; matlab-ts-mode--ei-bol2loc-map:
;;   Key `pos-bol', value is a location, loc, in line-nodes result from
;;   `matlab-ts-mode--ei-line-nodes-in-region' such that loc is the first location that satisfies:
;;   pos-bol <= (treesit-node-start (caar loc))
;;
;;   Given foo.m
;;      L1: a = 1;
;;      L2:
;;      L3:
;;      L4: % comment line 1
;;      L5: % comment line 2
;;      L6:
;;      L7: b = 2;
;;   M-x matlab-ei-utils-print-bol2loc-map
;;      -->matlab-ts-mode--ei-bol2loc-map
;;         L1  : point     1 (#<treesit-node identifier in 1-2> . "identifier")
;;         L2  : point     8 (#<treesit-node comment in 10-43> . "comment")
;;         L3  : point     9 (#<treesit-node comment in 10-43> . "comment")
;;         L4  : point    10 (#<treesit-node comment in 10-43> . "comment")
;;         L5  : point    27 (#<treesit-node identifier in 45-46> . "identifier")
;;         L6  : point    44 (#<treesit-node identifier in 45-46> . "identifier")
;;         L7  : point    45 (#<treesit-node identifier in 45-46> . "identifier")

(defvar-local matlab-ts-mode--ei-bol2loc-map nil)

(defun matlab-ts-mode--ei-bol2loc-setup (beg end region-nodes)
  "Use BEG END REGION-NODES to populate `matlab-ts-mode--ei-bol2loc-map'."
  (setq matlab-ts-mode--ei-bol2loc-map (make-hash-table))
  (save-excursion
    (let ((ptr region-nodes))
      (goto-char beg)
      (while (and ptr (< (point) end) (not (eobp)))

        (let* ((bol-pt (pos-bol))
               (node (cdar ptr))
               (node-pt (treesit-node-start node)))
          (while (and ptr (< node-pt bol-pt))
            (setq ptr (cdr ptr)
                  node (cdar ptr)
                  node-pt (treesit-node-start node)))
          (when ptr
            (puthash bol-pt ptr matlab-ts-mode--ei-bol2loc-map))
          (forward-line))))))

(defvar-local matlab-ts-mode--ei-errors-map nil) ;; Key (pos-bol), value t on an error line.

;; `matlab-ts-mode--ei-m-matrix-pos-bol-map'
;;   For each line of a "m-matrix", a multi-line matrix with one row per line ignoring blank and
;;   comment lines, map the key, which is `pos-bol' to:
;;     (cons 'numeric-m-matrix MATRIX-NODE-POS-BOL)  for numeric matrices when the row line
;;                                                     contains matrix elements, where
;;                                                     MATRIX-NODE-POS-BOL is the `pos-bol' of
;;                                                     the matrix node's start position
;;     (cons 'numeric-m-matrix 'empty)               for numeric matrices when the line is blank
;;                                                     or a comment / ellipsis line
;;     '(non-numeric-m-matrix)                       for non-numeric matrices
;;   from `matlab-ts-mode--ei-classify-matrix'.
;;   Note, 'not-a-m-matrix classifications are not recorded.
(defvar-local matlab-ts-mode--ei-m-matrix-pos-bol-map nil)

;; `matlab-ts-mode--ei-m-matrix-node-map'
;;   For each matrix classified as 'numeric-m-matrix, map the key MATRIX-NODE-POS-BOL
;;   (the `pos-bol' of the matrix node's start position) to the
;;   (MATRIX-TYPE . COLUMN-WIDTHS) cons pair returned by `matlab-ts-mode--ei-classify-matrix'.
(defvar-local matlab-ts-mode--ei-m-matrix-node-map nil)

(defconst matlab-ts-mode--ei-non-numeric-re
  "[^a-zA-Z0-9_.~ \t,;+\n-]"
  "Regexp matching a character that cannot appear in a numeric matrix row.
Numeric rows contain word tokens (identifiers, numbers including hex
and binary literals), unary operators, commas, and semicolons.
Expression-forming characters such as brackets, quotes, and `@'
indicate non-numeric content.")

(defconst matlab-ts-mode--ei-numeric-entry-re
  "[+~-]*[a-zA-Z0-9][a-zA-Z0-9_.+~-]*"
  "Regexp matching a single numeric entry in a matrix row.
Matches word tokens with optional leading unary operators.  Covers
decimal, exponential, hex (0xFF), binary (0b1010), signed/unsigned with
numbers with word size, \(0xFFs8), identifiers (inf, nan, pi, etc.).")

(defun matlab-ts-mode--ei-row-entry-count (row-node)
  "Count the number of entries in ROW-NODE.
Entries are children that are not separators (comma, semicolon),
comments, or line continuations."
  (let ((child-count (treesit-node-child-count row-node))
        (idx 0)
        (entry-count 0))
    (while (< idx child-count)
      (let ((child-type (treesit-node-type (treesit-node-child row-node idx))))
        (unless (or (string= child-type ",")
                    (string= child-type ";")
                    (string= child-type "comment")
                    (string= child-type "line_continuation"))
          (setq entry-count (1+ entry-count))))
      (setq idx (1+ idx)))
    entry-count))

(defun matlab-ts-mode--ei-valid-non-numeric-p (matrix-node)
  "Return non-nil if MATRIX-NODE is a valid non-numeric m-matrix.
A valid non-numeric m-matrix has uniform column counts across all
rows, each row on its own line, and no row spanning multiple lines.
Uses tree-sitter children to inspect the matrix structure."
  (let ((child-count (treesit-node-child-count matrix-node))
        (idx 0)
        (expected-cols nil)
        (prev-row-line nil)
        (valid t))
    (while (and valid (< idx child-count))
      (let* ((child (treesit-node-child matrix-node idx))
             (child-type (treesit-node-type child)))
        (when (string= child-type "row")
          (let ((cols (matlab-ts-mode--ei-row-entry-count child))
                (row-start-line (line-number-at-pos (treesit-node-start child)))
                (row-end-line (line-number-at-pos (treesit-node-end child))))
            ;; Check uniform column count
            (if (null expected-cols)
                (setq expected-cols cols)
              (when (/= cols expected-cols)
                (setq valid nil)))
            ;; Check row is on a single line
            (when (and valid (/= row-start-line row-end-line))
              (setq valid nil))
            ;; Check no two rows share a line
            (when (and valid prev-row-line (= row-start-line prev-row-line))
              (setq valid nil))
            (setq prev-row-line row-start-line))))
      (setq idx (1+ idx)))
    valid))

(defun matlab-ts-mode--ei-classify-matrix (matrix-node)
  "Classify MATRIX-NODE and return a cons pair (MATRIX-TYPE . COLUMN-WIDTHS).
MATRIX-TYPE is one of:
  \\='not-a-m-matrix
     Single-line matrix, multi-line with multiple rows on one line,
     or rows with non-uniform column counts.
  \\='numeric-m-matrix
     Multi-line matrix assignment where each row is on its own line and
     contains only non-expression entries (numbers, identifiers,
     unary operators).
  \\='non-numeric-m-matrix
     Multi-line matrix assignment where each row is on its own line but
     contains non-numeric entries.

COLUMN-WIDTHS is a list of per-column maximum entry widths when
MATRIX-TYPE is \\='numeric-m-matrix, otherwise nil.

Uses `re-search-forward' on the buffer text to identify
\\='numeric-m-matrix for performance.  When the regexp scan does not
confirm numeric, falls back to `matlab-ts-mode--ei-valid-non-numeric-p'
which uses tree-sitter children nodes to determine
\\='non-numeric-m-matrix vs \\='not-a-m-matrix."

  (let ((mat-start (treesit-node-start matrix-node))
        (mat-end (treesit-node-end matrix-node)))
    ;; Check if single-line matrix or if the first/last line has extra language elements after the
    ;; matrix delimiter, "]" and on the same line.  For example both of these are 'not-a-m-matrix:
    ;;    m1 = [1 2; 3 4];
    ;;    m2 = [1 2;
    ;;          3 4]; m3 = [1 2; 3 4];        // Simplification: don't align
    (if (or
         ;; Single-line matrix, e.g. m1 = [1 2; 3 4];
         (save-excursion (= (progn (goto-char mat-start) (pos-bol))
                            (progn (goto-char mat-end) (pos-bol))))
         ;; OR extra content after "]" on the last line? We shouldn't align these because
         ;;    it wouldn't look nice (and be complex to implement). Example:
         ;;       m2 = [1 2;
         ;;             3 4]; m3 = [1 2; 3 4];
         (not (matlab-ts-mode--ei-matrix-ends-on-line matrix-node)))
        '(not-a-m-matrix)
      ;; Multi-line matrix: scan buffer text line by line to classify.
      ;; The text scan is only reliable for numeric content.  When
      ;; non-numeric content is detected, stop the text scan and let
      ;; tree-sitter validate the matrix structure, since string
      ;; literals and other expressions can contain characters like
      ;; ";", "...", "[", "]" that confuse text-based checks.
      (save-excursion
        (goto-char mat-start)
        (let ((is-numeric t)    ;; assume
              (is-valid t)      ;; assume
              (num-cols nil)    ;; expected column count (set from first row with entries)
              (col-widths nil)) ;; list of max widths per column
          (while (and is-valid is-numeric (< (point) mat-end))
            (let* ((lstart (point))
                   (lend (min (pos-eol) mat-end)))
              ;; Find effective end of code on this line (before % comment)
              (goto-char lstart)
              (let* ((eff-end (if (re-search-forward "%" lend t)
                                 (match-beginning 0)
                               lend))
                     ;; Find line continuation (...)
                     (cont-pos (progn
                                 (goto-char lstart)
                                 (when (re-search-forward "\\.\\.\\." eff-end t)
                                   (match-beginning 0))))
                     (code-end (if cont-pos cont-pos eff-end)))
                ;; Compute scan boundaries that skip the matrix's own
                ;; "[" on the first line and "]" on the last line.
                (let ((scan-start (if (= lstart mat-start) (1+ lstart) lstart))
                     (scan-end (if (and (>= (1- code-end) lstart)
                                        (eq (char-after (1- code-end)) ?\]))
                                   (1- code-end)
                                 code-end)))
                ;; Check for non-numeric content first.  When detected,
                ;; stop the text scan immediately.  Text-based validity
                ;; checks below are unreliable for non-numeric content.
                (goto-char scan-start)
                (if (re-search-forward matlab-ts-mode--ei-non-numeric-re
                                      scan-end t)
                    (setq is-numeric nil)
                  ;; Numeric line: perform text-based validity checks.
                  ;; Check for multiple rows on one line:
                  ;; A ";" followed by non-whitespace content (other than "]") means
                  ;; two rows share this line.
                  (goto-char lstart)
                  (when (re-search-forward ";[ \t]*[^ \t\n]" code-end t)
                    (unless (eq (char-before) ?\])
                      (setq is-valid nil)))
                  ;; Collect entry widths for this row
                  (when is-valid
                    (goto-char lstart)
                    (let ((col 0)
                          (row-widths nil))
                      (while (re-search-forward matlab-ts-mode--ei-numeric-entry-re
                                                code-end t)
                        (let ((w (- (match-end 0) (match-beginning 0))))
                          (push w row-widths))
                        (setq col (1+ col)))
                      (when (> col 0)
                        ;; Check for multi-line row: a line with entries
                        ;; and "..." but no ";" may continue onto the next
                        ;; line.  Allow it when the entry count matches the
                        ;; established column count (row is complete and
                        ;; "..." is just a trailing continuation/comment).
                        (when (and cont-pos
                                   (save-excursion
                                     (goto-char lstart)
                                     (not (re-search-forward ";" cont-pos t))))
                          (when (or (null num-cols) (/= col num-cols))
                            (setq is-valid nil)))
                        ;; Check uniform column count
                        (when is-valid
                          (if (null num-cols)
                              (setq num-cols col)
                            (when (/= col num-cols)
                              (setq is-valid nil))))
                        (unless (not is-valid)
                          (setq row-widths (nreverse row-widths))
                          ;; Merge row-widths into col-widths (element-wise max)
                          (if (null col-widths)
                              (setq col-widths row-widths)
                            (let ((cw col-widths)
                                  (rw row-widths))
                              (while (and cw rw)
                                (when (> (car rw) (car cw))
                                  (setcar cw (car rw)))
                                (setq cw (cdr cw)
                                      rw (cdr rw))))))))))
                )) ;; end of let scan-start/scan-end, if non-numeric check
              ;; Advance to the next line
              (goto-char lend)
              (forward-line)))
          ;; result
          (if (and is-numeric is-valid)
              ;; Text scan confirmed numeric with valid structure.
              (cons 'numeric-m-matrix col-widths)
            ;; Not numeric: use tree-sitter to validate matrix structure.
            (if (matlab-ts-mode--ei-valid-non-numeric-p matrix-node)
                '(non-numeric-m-matrix)
              '(not-a-m-matrix))))))))

(defun matlab-ts-mode--ei-mark-m-matrix-lines (matrix-node)
  "Classify MATRIX-NODE and populate matrix maps.
Add lines to `matlab-ts-mode--ei-m-matrix-pos-bol-map' and, for
\\='numeric-m-matrix, add the node to `matlab-ts-mode--ei-m-matrix-node-map'."
  (let* ((result (matlab-ts-mode--ei-classify-matrix matrix-node))
         (matrix-type (car result)))
    (when (not (eq matrix-type 'not-a-m-matrix))
      (let* ((mat-start (treesit-node-start matrix-node))
             (mat-end (treesit-node-end matrix-node))
             (matrix-node-pos-bol (save-excursion (goto-char mat-start) (pos-bol)))
             (pos-bol-value (if (eq matrix-type 'numeric-m-matrix)
                                (cons 'numeric-m-matrix matrix-node-pos-bol)
                              '(non-numeric-m-matrix))))
        (when (eq matrix-type 'numeric-m-matrix)
          (puthash matrix-node-pos-bol result matlab-ts-mode--ei-m-matrix-node-map))
        (save-excursion
          (goto-char mat-start)
          (forward-line 0)
          (while (and (<= (point) mat-end) (not (eobp)))

            (puthash (point)
                     (if (and (eq matrix-type 'numeric-m-matrix)
                              (save-excursion
                                (when (< (point) mat-start)
                                  (goto-char (1+ mat-start)))
                                (looking-at (rx (0+ (or " " "\t")) (or eol "%" "..." "]")))))
                         (cons 'numeric-m-matrix 'empty)
                       pos-bol-value)
                     matlab-ts-mode--ei-m-matrix-pos-bol-map)
            (forward-line)))))))

(defun matlab-ts-mode--ei-mark-error-lines (error-node)
  "Add lines of ERROR-NODE to `matlab-ts-mode--ei-errors-map'."
  (let* ((error-start-pt (treesit-node-start error-node))
         (error-end-pt (treesit-node-end error-node)))
    (save-excursion
      (goto-char error-start-pt)
      (while (and (<= (point) error-end-pt) (not (eobp)))
        (puthash (pos-bol) t matlab-ts-mode--ei-errors-map)
        (forward-line)))))

(defvar matlab-ts-mode--ei-error-query (when (treesit-available-p)
                                         (treesit-query-compile 'matlab '((ERROR) @e))))

(defun matlab-ts-mode--ei-query-errors ()
  "Query tree-sitter for ERROR's and populate `matlab-ts-mode--ei-errors-map'."
  (let ((curr-err-map matlab-ts-mode--ei-errors-map)
        (error-nodes (treesit-query-capture (treesit-buffer-root-node 'matlab)
                                            matlab-ts-mode--ei-error-query nil nil t)))
    (setq matlab-ts-mode--ei-errors-map (make-hash-table))
    (dolist (error-node error-nodes)
      (matlab-ts-mode--ei-mark-error-lines error-node))
    (let ((err-map matlab-ts-mode--ei-errors-map))
      (setq matlab-ts-mode--ei-errors-map curr-err-map)
      ;; result
      err-map)))

(defvar matlab-ts-mode--ei-all-nodes-query
  (when (treesit-available-p)
    (treesit-query-compile
     'matlab
     `(
       ;; Matrix nodes to identify multi-row matrices (m-matrices) for alignment
       ((matrix) @matrix)

       ;; Named nodes
       ((identifier) @identifier)
       ((number) @number)
       ((string) @string)
       ((comment) @comment)
       ((line_continuation) @line_continuation)
       ((command_argument) @command_argument)
       ((command_name) @command_name)
       ((dimensions) @dimensions)
       ((ERROR) @error)

       ;; Punctuation & delimiters
       ("," @comma) (";" @semicolon) ("." @dot)
       ("(" @lparen) (")" @rparen)
       ("[" @lbrack) ("]" @rbrack) ("{" @lbrace) ("}" @rbrace)

       ;; Arithmetic & assignment operators
       ("+" @plus) ("-" @minus) ("*" @star) ("/" @slash) ("\\" @backslash)
       ("^" @caret) (":" @colon) ("=" @assign)

       ;; Dot operators
       (".*" @dotStar) ("./" @dotSlash) (".\\" @dotBackslash) (".^" @dotCaret)
       (".?" @dotQuestion) (".'" @dotTranspose)

       ;; Comparison operators
       ("<" @lt) (">" @gt) ("<=" @leq) (">=" @geq) ("==" @eqeq) ("~=" @neq)

       ;; Logical operators
       ("&" @amp) ("|" @pipe) ("&&" @and2) ("||" @or2)

       ;; Other operators
       ("~" @tilde) ("@" @at) ("?" @question)

       ;; String or transpose
       ("'" @singleTick)

       ;; Keywords
       ("arguments" @kw_arguments)
       ("break" @kw_break)
       ("case" @kw_case)
       ("catch" @kw_catch)
       ("classdef" @kw_classdef)
       ("continue" @kw_continue)
       ("else" @kw_else)
       ("elseif" @kw_elseif)
       ("end" @kw_end)
       ("enumeration" @kw_enumeration)
       ("events" @kw_events)
       ("for" @kw_for)
       ("function" @kw_function)
       ("global" @kw_global)
       ("if" @kw_if)
       ("methods" @kw_methods)
       ("otherwise" @kw_otherwise)
       ("parfor" @kw_parfor)
       ("persistent" @kw_persistent)
       ("properties" @kw_properties)
       ("return" @kw_return)
       ("spmd" @kw_spmd)
       ("switch" @kw_switch)
       ("try" @kw_try)
       ("while" @kw_while)

       ;; classdef property get and set method start nodes
       ;; (https://www.mathworks.com/help/matlab/matlab_oop/property-set-methods.html)
       ("get." @kw_get_dot)
       ("set." @kw_set_dot)
       ))))

;; Map capture symbols to their node-type strings for the spacing table.
;; Only symbols whose type string differs from (symbol-name sym) need entries;
;; for the rest we derive the string from the grammar node type at init time.
(defvar matlab-ts-mode--ei-capture-type-map
  (let ((ht (make-hash-table :test 'eq :size 80)))
    ;; Named nodes — capture symbol matches type string
    (dolist (sym '(identifier number string comment line_continuation
                              command_argument command_name))
      (puthash sym (symbol-name sym) ht))
    ;; Punctuation & delimiters
    (puthash 'comma "," ht)     (puthash 'semicolon ";" ht)
    (puthash 'dot "." ht)
    (puthash 'lparen "(" ht)    (puthash 'rparen ")" ht)
    (puthash 'lbrack "[" ht)    (puthash 'rbrack "]" ht)
    (puthash 'lbrace "{" ht)    (puthash 'rbrace "}" ht)
    ;; Arithmetic & assignment
    (puthash 'plus "+" ht)      (puthash 'minus "-" ht)
    (puthash 'star "*" ht)      (puthash 'slash "/" ht)
    (puthash 'backslash "\\" ht)
    (puthash 'caret "^" ht)     (puthash 'colon ":" ht)
    (puthash 'assign "=" ht)
    ;; Dot operators
    (puthash 'dotStar ".*" ht)       (puthash 'dotSlash "./" ht)
    (puthash 'dotBackslash ".\\" ht) (puthash 'dotCaret ".^" ht)
    (puthash 'dotQuestion ".?" ht)   (puthash 'dotTranspose ".'" ht)
    ;; Comparison
    (puthash 'lt "<" ht)        (puthash 'gt ">" ht)
    (puthash 'leq "<=" ht)      (puthash 'geq ">=" ht)
    (puthash 'eqeq "==" ht)     (puthash 'neq "~=" ht)
    ;; Logical
    (puthash 'amp "&" ht)       (puthash 'pipe "|" ht)
    (puthash 'and2 "&&" ht)     (puthash 'or2 "||" ht)
    ;; Other operators
    (puthash 'tilde "~" ht)     (puthash 'at "@" ht)
    (puthash 'question "?" ht)  (puthash 'singleTick "'" ht)
    ;; Keywords
    (puthash 'kw_arguments "arguments" ht)
    (puthash 'kw_break "break" ht)
    (puthash 'kw_case "case" ht)
    (puthash 'kw_catch "catch" ht)
    (puthash 'kw_classdef "classdef" ht)
    (puthash 'kw_continue "continue" ht)
    (puthash 'kw_else "else" ht)
    (puthash 'kw_elseif "elseif" ht)
    (puthash 'kw_end "end" ht)
    (puthash 'kw_enumeration "enumeration" ht)
    (puthash 'kw_events "events" ht)
    (puthash 'kw_for "for" ht)
    (puthash 'kw_function "function" ht)
    (puthash 'kw_global "global" ht)
    (puthash 'kw_if "if" ht)
    (puthash 'kw_methods "methods" ht)
    (puthash 'kw_otherwise "otherwise" ht)
    (puthash 'kw_parfor "parfor" ht)
    (puthash 'kw_persistent "persistent" ht)
    (puthash 'kw_properties "properties" ht)
    (puthash 'kw_return "return" ht)
    (puthash 'kw_spmd "spmd" ht)
    (puthash 'kw_switch "switch" ht)
    (puthash 'kw_try "try" ht)
    (puthash 'kw_while "while" ht)
    (puthash 'kw_get_dot "get." ht)
    (puthash 'kw_set_dot "set." ht)
    ht))

(defun matlab-ts-mode--ei-line-nodes-in-region (beg end)
  "Get line nodes in region BEG to END.
Line nodes are the language elements that we electric indent in the
line.  These are typically leaf nodes except for nodes like strings
which are not leaf nodes.

Returns list with elements (MODIFIED-NODE-TYPE . NODE) where
MODIFIED-NODE-TYPE is the NODE type adjusted.  For example, when NODE
parent is a string and NODE is the string start characters, we return
the parent string node.  Another example: when NODE is a \"+\" and
parent is a unary_operator, we return MODIFIED-NODE-TYPE to be unary-op
even though the node type is \"+\"."

  (setq matlab-ts-mode--ei-errors-map (make-hash-table))
  (setq matlab-ts-mode--ei-m-matrix-pos-bol-map (make-hash-table))
  (setq matlab-ts-mode--ei-m-matrix-node-map (make-hash-table))

  (let* ((region-nodes (treesit-query-capture (treesit-buffer-root-node 'matlab)
                                              matlab-ts-mode--ei-all-nodes-query beg end))
         dimensions-node ;; we use this to track the "number" and ":" with in properties dimension
         (prev-ptr region-nodes)
         (ptr prev-ptr))

    (while ptr
      (let* ((capture (caar ptr))
             (node (cdar ptr))
             (node-type (gethash capture matlab-ts-mode--ei-capture-type-map)))

        (cond
         ;; Case: ERROR node
         ((eq capture 'error)
          (matlab-ts-mode--ei-mark-error-lines node)
          (setq node nil))

         ;; Case: matrix node to identify multi-line matrices for alignment
         ((eq capture 'matrix)
          (matlab-ts-mode--ei-mark-m-matrix-lines node)
          (setq node nil))

         ;; Case: track when in a property dimensions node
         ((eq capture 'dimensions)
          (setq dimensions-node node
                node nil))

         ;; Case: arguments fcn keyword: arguments (1, :) {mustBeNumeric}
         ;;                                        ^
         ((and dimensions-node (eq capture 'lparen))
          (setq node-type "dim-("))

         ;; Case: property dimensions
         ;;   foo1 (1, :) {mustBeNumeric, mustBeReal} = [0, 0, 0];
         ((and dimensions-node (or (eq capture 'number) (eq capture 'colon)))
          (setq node-type "prop-dim"))

         ;; Case: lambda:     @(x) ((ischar(x) || isstring(x)))
         ;;                      ^
         ;;       properties: foo1 (1, :) {mustBeNumeric, mustBeReal} = [0, 0, 0];
         ;;                             ^
         ((eq capture 'rparen)
          (if dimensions-node
              (setq dimensions-node nil ;; close of dimension-node
                    node-type "dim-)")
            (when (string= (treesit-node-type (treesit-node-parent node)) "lambda")
              (setq node-type "lambda-)"))))

         ;; Case: ' string delimiter — ignore (transpose is kept)
         ((and (eq capture 'singleTick)
               (string= (treesit-node-type (treesit-node-parent node)) "string"))
          (setq node nil))

         ;; Case: prop-id, prop-class-id, enum-id
         ((eq capture 'identifier)
          (let* ((parent (treesit-node-parent node))
                 (parent-type (treesit-node-type parent)))
            (cond
             ;; arguments
             ;;     p1              % property
             ;; end
             ;; classdef
             ;;     properties
             ;;         p1          % property
             ;;     end
             ;; end
             ((string= parent-type "property") ;; propertyWithOutDot?
              (if (equal (treesit-node-child parent 0) node)
                  (setq node-type "prop-id")
                (setq node-type "prop-class-id")))

             ;; arguments
             ;;     opts.foo string                               % property_name
             ;;     opts.onOff (1,1) matlab.lang.OnOffSwitchState % two property_name's
             ;; end
             ;;
             ;; classdef foo < a & b.c.d                          % two property_name's to skip
             ;; end
             ((string= parent-type "property_name") ;; property.nameWithDot?
              (if (equal (treesit-node-child (treesit-node-parent parent) 0) parent)
                  (setq node-type "prop-id")
                (setq node-type "prop-class-id")))

             ;; enumeration
             ;;     e1 (1)   % e1 is an identifier node that we give modified type: enum-id
             ;; end
             ((string= parent-type "enum")
              (setq node-type "enum-id")))))

         ;; Case: unary operator sign, + or -, e.g. [0 -e] or g = - e
         ((or (eq capture 'plus) (eq capture 'minus))
          (let* ((parent (treesit-node-parent node))
                 (parent-type (treesit-node-type parent)))
            (when (and (string= parent-type "unary_operator")
                       (equal (treesit-node-child parent 0) node))
              (setq node-type "unary-op"))))

         ;; Case: super-class constructor call
         ;;  obj@myUtils.super;
         ((eq capture 'at)
          (let* ((parent (treesit-node-parent node))
                 (parent-type (treesit-node-type parent)))
            (when (string= parent-type "function_call")
              (setq node-type "@-fcn-call"))))

         ;; Case: events, enumeration, methods, arguments "commands" and not keywords
         ((memq capture '(kw_events kw_enumeration kw_methods kw_arguments))
          (when (= (treesit-node-child-count node) 0) ;; no children == command
            (let* ((parent (treesit-node-parent node))
                   (parent-type (treesit-node-type parent)))
              (when (string= parent-type "identifier")
                ;; TopTester: electric_indent_inspect_keyword_commands.m
                ;; TopTester: electric_indent_inspect_keyword_commands2.m
                (setq node-type (concat node-type "-fcn")))))))

        (if (and node
                 ;; See: electric_indent_xr_prop_hidden_nodes.m: empty nodes in { }
                 ;; See: electric_indent_example.m: figure       ;% Create a new figure
                 ;;                                              ^ <== empty command_argument
                 (or (not (= (treesit-node-start node) (treesit-node-end node)))
                     (eq capture 'comma)))
            ;; Keep the node and change the capture name to the node-type string
            (progn
              (setcar (car ptr) node-type)
              (setq prev-ptr ptr ;; advance
                    ptr (cdr ptr)))
          ;; Else remove the node
          (if (eq ptr region-nodes)
              (setq region-nodes (cdr ptr)
                    ptr region-nodes)
            (setq ptr (cdr ptr))
            (setcdr prev-ptr ptr)))))

    (matlab-ts-mode--ei-bol2loc-setup beg end region-nodes)

    ;; Result may be nil (consider a region of only blank lines)
    region-nodes))

;; This is setup before `matlab-ts-mode--ei-move-to-and-get-node-pair' is used
(defvar-local matlab-ts-mode--ei-line-nodes nil)

(defvar-local matlab-ts-mode--ei-line-nodes-loc nil)
(defvar-local matlab-ts-mode--ei-line-nodes-eat-comma nil)

(cl-defun matlab-ts-mode--ei-move-to-and-get-node-pair ()
  "Move to and return (MODIFIED-NODE-TYPE . NODE) in line.
Assumes point is at end of an existing node or at beginning of line.
Leverages `matlab-ts-mode--ei-line-nodes-loc' and updates it.

Returned MODIFIED-NODE-TYPE and NODE will be nil if no next node before
end-of-line.  MODIFIED-NODE-TYPE is computed by
`matlab-ts-mode--ei-line-nodes-in-region' and is used in
`matlab-ts-mode--ei-spacing'."

  ;; Move point to next node start
  (when (looking-at "[ \t]")
    (if (re-search-forward "[^ \t]" nil (pos-eol))
        (backward-char)
      (cl-return-from matlab-ts-mode--ei-move-to-and-get-node-pair)))

  (let* ((head-pair (car matlab-ts-mode--ei-line-nodes-loc))
         (pt (point)))

    (cl-loop while (let* ((node (cdr head-pair))
                          (node-end (treesit-node-end node)))
                     (and node
                          (or (> pt node-end)
                              (and (= pt node-end)
                                   ;; Are we at a hidden node, e.g.:
                                   ;;   a = [1 2 -3]
                                   ;;          ^    hidden comma node here where start == end
                                   (if (= (treesit-node-start node) node-end)
                                       ;; Keep the hidden node (e.g. comma) unless we are eating it.
                                       ;; See: electric_indent_eat_hidden_comma.m
                                       ;;   a = [1 ...  // we use the hidden comma after the 1 node
                                       ;;        ...
                                       ;;        2];    // hidden comma is before the 2 node
                                       (when (and matlab-ts-mode--ei-line-nodes-eat-comma
                                                  (string= (treesit-node-type node) ","))
                                         (setq matlab-ts-mode--ei-line-nodes-eat-comma nil)
                                         t)
                                     ;; Else node start != end, so move forward.
                                     t)))))
             do
             (setq matlab-ts-mode--ei-line-nodes-loc (cdr matlab-ts-mode--ei-line-nodes-loc)
                   head-pair (car matlab-ts-mode--ei-line-nodes-loc)))

    (if-let* ((head-node (cdr head-pair)))
        (let ((head-node-start (treesit-node-start head-node)))
          (if (= head-node-start (treesit-node-end head-node))
              ;; Hidden comma has start == end and we just used it, so move our location
              (setq matlab-ts-mode--ei-line-nodes-loc (cdr matlab-ts-mode--ei-line-nodes-loc))
            ;; Else, see if we moved to the next line
            (when (> head-node-start (pos-eol))
              (setq head-pair nil)))))

    ;; Result which may be nil
    head-pair))

(defun matlab-ts-mode--ei-assert-line-match (new-line orig-line)
  "Assert NEW-LINE matches ORIG-LINE ignoring spaces, tabs, and commas."
  ;; Spaces may be added or removed. Commas may be added to new-line, so ignore them.
  (let* ((new-line-no-spaces-or-commas (replace-regexp-in-string "[ \t,]" "" new-line))
         (orig-line-no-spaces-or-commas (replace-regexp-in-string "[ \t,]" "" orig-line)))

    (when (not (string= new-line-no-spaces-or-commas orig-line-no-spaces-or-commas))
      (matlab-ts-mode--assert-msg
       (format "line-content-no-space-mismatch new: \"%s\" !EQ orig: \"%s\" at line %d in %s"
               new-line-no-spaces-or-commas orig-line-no-spaces-or-commas
               (line-number-at-pos (point)) (buffer-name))))))

(defun matlab--ei-offset (&optional pt)
  "Character based column offset in line, starting at 0.
If optional point, PT is specified, return line offset for PT, otherwise
the current `point'.  This is like `current-column' but character based
and not \"visual\" based."
  (if pt
      (- pt (save-excursion (goto-char pt) (pos-bol)))
    (- (point) (pos-bol))))

(defvar-local matlab--eilb nil) ;; Buffer used to create new electric indent line, ei-line

(defmacro matlab--eilb-length ()
  "Current ei-line length."
  '(with-current-buffer matlab--eilb (1- (point))))

(defmacro matlab--eilb-content ()
  "Current ei-line ."
  '(with-current-buffer matlab--eilb (buffer-string)))

(defun matlab--eilb-kill ()
  "Kill the `matlab--eilb' buffer."
  (when matlab--eilb
    (kill-buffer matlab--eilb)
    (setq-local matlab--eilb nil)))

(defun matlab--eilb-setup ()
  "Initialize `matlab-ts-mode--eilb' buffer used to electric indent.
Return t."
  (if matlab--eilb
      (with-current-buffer matlab--eilb
        (erase-buffer))
    ;; Else create matlab--eilb
    (let ((eilb-name (let ((bn (buffer-name)))
                       (concat (when (not (string-match (rx bos " *") bn)) " *")
                               bn "-matlab-ts-mode--ei-line*"))))
      (setq-local matlab--eilb (get-buffer-create eilb-name))
      (with-current-buffer matlab--eilb
        (buffer-disable-undo))))
  t)

(defun matlab--eilb-add-node-text (node extra-chars &optional n-spaces-to-append)
  "Update `matlab--eilb' with NODE text and more.
EXTRA-CHARS string is appended to EL-LINE after NODE text.
EXTRA-CHARS, when \\='(string), the string is appended to last
non-whitespace in EL-LINE, then NODE text is appended.
N-SPACES-TO-APPEND is the number of spaces to append between nodes."

  (let* ((node-end (treesit-node-end node))
         (eol-pt (pos-eol))
         (last-pt (if (< node-end eol-pt) node-end eol-pt))
         (node-text (buffer-substring-no-properties (treesit-node-start node) last-pt)))

    (with-current-buffer matlab--eilb

      (when (listp extra-chars)
        ;; Consider: foo1 = {'one', 'two' ...
        ;;                  ...
        ;;              'three' 'four' 'five'};
        ;; After the 'two' node, we have two line_continuation's, then the invisible comma (",")
        ;; This is detected and extra-chars will be '(",")
        ;; TopTester: electric_indent_cell_no_comma_before_ellipsis.m
        (goto-char (point-min))
        (if (re-search-forward (rx (1+ " ") eol))
            (progn
              (replace-match (concat (car extra-chars) "\\&") t)
              (goto-char (point-max)))
          (goto-char (point-max))
          (insert (car extra-chars)))

        (setq extra-chars ""))

      (insert node-text extra-chars)

      (when (and n-spaces-to-append (> n-spaces-to-append 0))
        (insert (make-string n-spaces-to-append ? ))))))

;; matlab-ts-mode--ei-tmp-buf-indent is non-nil if doing m-matrix indent (in this case we know there
;; are no ERROR nodes).
(defvar-local matlab-ts-mode--ei-tmp-buf-indent nil)

(defun matlab-ts-mode--ei-no-elements-to-indent ()
  "Return t if no elements in the current line to indent.
Assumes that current point is at `back-to-indentation'."
  (or
   ;; (1) Nothing to indent in line if it's a comment or ellipsis-line.
   (let ((first-node-type (or (treesit-node-type (treesit-node-at (point) 'matlab)) "")))
     (string-match-p (rx bos (or "line_continuation" "comment") eos) first-node-type))
   ;; (2) Nothing to indent if syntax error on the line.
   (gethash (pos-bol) matlab-ts-mode--ei-errors-map)))

(defun matlab-ts-mode--ei-node-extra-chars (node node-end next-node-start)
  "Get extra chars for NODE after NODE-END and before NEXT-NODE-START."

  (let ((node-type (treesit-node-type node)))
    (cond

     ;; Case: invisible comma, make it visible by returning the comma (",")
     ((and (string= node-type ",")
           (= (treesit-node-start node) (treesit-node-end node)))
      ;; Make invisible comma visible by returning it.
      ",")

     ;; Case: invisible comma to insert at end of array row
     ;;           foo = {'one', 'two' ...
     ;;                              ^   missing comma, return a comma to have it inserted
     ((and (string= node-type "line_continuation")
           (string= (treesit-node-type (treesit-node-parent node)) "row"))
      (let ((next-node (treesit-node-next-sibling node))  ;; after the line continuation
            (ans ""))
        (while (and next-node
                    (string= (treesit-node-type next-node) "line_continuation"))
          (setq next-node (treesit-node-next-sibling next-node)))

        (when next-node
          (let ((row-item next-node)
                (next-parent (treesit-node-parent next-node)))
            (while (and next-parent
                        (not (string= (treesit-node-type next-parent) "row")))
              (setq row-item next-parent
                    next-parent (treesit-node-parent next-parent)))

            (when next-parent ;; next parent is a row
              ;; Emacs 30.1 build 1 returns the hidden comma when using
              ;; treesit-node-next-sibling. Emacs 30.1 build 2 doesn't, but does for
              ;; treesit-node-prev-sibling.
              (let ((row-item-type (treesit-node-type row-item)))
                (when (not (string= row-item-type ","))
                  (setq row-item (treesit-node-prev-sibling row-item))
                  (setq row-item-type (treesit-node-type row-item)))
                (when (and (equal row-item-type ",")
                           (= (treesit-node-start row-item) (treesit-node-end row-item)))
                  (setq matlab-ts-mode--ei-line-nodes-eat-comma t)
                  (setq ans '(","))  ;; list means prefix to NODE text
                  )))))
        ans))

     ;; Case: Handle ignored characters, e.g. ";" in matrices where node="]", next-node="["
     ;;   [[1, 2]; [3, 4]]
     ;;         ^  ^
     (t
      (let ((extra-chars ""))
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
        extra-chars)))))

(defun matlab-ts-mode--ei-update-line-node-types (line-node-types node node-type)
  "Append NODE-TYPE of NODE to LINE-NODE-TYPES."
  (if (and (string= node-type ";")
           (string= (treesit-node-type (treesit-node-parent node)) "matrix"))
      ;; Ignore ';' row separator in matrix because these may be ignored by tree-sitter
      line-node-types
    (concat line-node-types (when line-node-types " ") node-type)))

(defun matlab-ts-mode--ei-insert-indent-level-spaces ()
  "Insert indent-level spaces for current line expanding tabs."
  (let ((spaces (buffer-substring-no-properties (pos-bol) (point))))
    (when (string-match "\t" spaces)
      (setq spaces (with-temp-buffer
                     (insert spaces)
                     (untabify (point-min) (point-max))
                     (buffer-string))))
    (with-current-buffer matlab--eilb
      (insert spaces))))

;; Internal variable that shouldn't be altered. It's used to avoid infinite recursion.
(defvar matlab-ts-mode--ei-align-enabled t)

(defun matlab-ts-mode--ei-nm-matrix-state (bol-pt)
  "Compute numeric-m-matrix state used in obtaining a new line for BOL-PT."
  (let* ((nm-pos-bol-value (when matlab-ts-mode--ei-align-enabled
                             (gethash bol-pt matlab-ts-mode--ei-m-matrix-pos-bol-map)))
         (nm-matrix-pos-bol (when (and nm-pos-bol-value
                                       (eq (car nm-pos-bol-value) 'numeric-m-matrix)
                                       (not (eq (cdr nm-pos-bol-value) 'empty)))
                              (cdr nm-pos-bol-value)))
         (nm-col-widths (when nm-matrix-pos-bol
                          (cdr (gethash nm-matrix-pos-bol
                                       matlab-ts-mode--ei-m-matrix-node-map))))
         (nm-first-col-extra (when nm-matrix-pos-bol
                               (matlab-ts-mode--ei-m-matrix-first-col-extra nm-matrix-pos-bol)))
         (nm-inside-matrix (when nm-matrix-pos-bol
                             ;; On the first line of the matrix, wait for "[" to start tracking.
                             ;; On subsequent row lines, we are already inside the matrix.
                             (not (= bol-pt nm-matrix-pos-bol)))))
    ;; Result
    (list nm-col-widths nm-first-col-extra nm-inside-matrix)))

(defconst matlab-ts-mode--ei-matrix-syntax-nodes
  (rx bos (or "," ";" "comment" "line_continuation" "[" "]") eos)
  "Node types that are used in the definition of a matrix.")

(defun matlab-ts-mode--ei-is-multi-line-matrix (matrix-node)
  "Is MATRIX-NODE multi-line matrix?
Note, this does not mean it's a multi-row matrix or an m-matrix.
It means the start line and end line are different."
  (save-excursion
    (not (= (progn (goto-char (treesit-node-start matrix-node)) (pos-bol))
            (progn (goto-char (treesit-node-end matrix-node)) (pos-bol))))))

(cl-defun matlab-ts-mode--ei-get-new-line (&optional start-node start-offset)
  "Get new line content with element spacing adjusted.
Optional START-NODE and START-OFFSET are used to compute new pt-offset,
the point offset in the line used to restore point after updating line.
Note, new line content may be same as current line.  Also computes
ORIG-LINE-NODE-TYPES which is a string containing the original line node types.

If no new line (e.g. blank, comment, or syntax error on line), nil is
returned, otherwise returns electric indent info, ei-info:

  (list NEW-LINE-CONTENT PT-OFFSET ORIG-LINE-NODE-TYPES FIRST-NODE-PAIR-IN-LINE)
        0                1         2                    3
where:
 - NEW-LINE-CONTENT is the indented line.  Note, during `indent-region',
   does not contain the final amount of leading whitespace because we do
   electric indent before `treesit-indent-region'.
 - PT-OFFSET is the new point offset in the NEW-LINE-CONTENT
 - ORIG-LINE-NODE-TYPES is the nodes in the original line when
   `matlab-ts-mode--indent-assert' is active, otherwise nil.
 - FIRST-NODE-PAIR-IN-LINE is (node . modified-node-type)"
  (save-excursion
    ;; Move to first non-whitespace character on the line
    (let ((have-non-empty-line (matlab-ts-mode--ei-fast-back-to-indentation)))
      (when (or (not have-non-empty-line)
                (matlab-ts-mode--ei-no-elements-to-indent))
        (cl-return-from matlab-ts-mode--ei-get-new-line)))

    ;; Setup for `matlab-ts-mode--ei-move-to-and-get-node-pair'
    (setq matlab-ts-mode--ei-line-nodes-loc (gethash (pos-bol) matlab-ts-mode--ei-bol2loc-map))
    (when (not matlab-ts-mode--ei-line-nodes-loc)
      (matlab-ts-mode--assert-msg "No pos-bol in matlab-ts-mode--ei-bol2loc-map"))

    ;; --- Loop inserting spaces between language elements ---
    ;;
    ;; Optimization: defer matlab--eilb-setup and matlab-ts-mode--ei-insert-indent-level-spaces.
    ;; Walk nodes in the current buffer and check whether the existing content already matches.
    ;; Only when a mismatch is detected, set up the eilb buffer, copy the already-verified prefix,
    ;; and continue building the rest in eilb.
    (let* (pt-offset ;; used in restoring point
           using-eilb ;; non-nil when we've switched to building in eilb
           (bol-pt (pos-bol))
           (pair (matlab-ts-mode--ei-move-to-and-get-node-pair))
           (node-type (or (car pair)
                          (cl-return-from matlab-ts-mode--ei-get-new-line)))
           (node (cdr pair))
           (first-node-pair pair)
           orig-line-node-types
           next2-pair ;; used when we have: (NODE-RE (NEXT-NODE-RE NEXT2-NODE-RE) N-SPACES-BETWEEN)
           next2-n-spaces-between
           (eol-pt (pos-eol))
           (indent-has-tabs (save-excursion
                              (let ((first-non-whitespace-char-in-line-pt (point)))
                                (goto-char bol-pt)
                                (re-search-forward "\t" first-non-whitespace-char-in-line-pt t))))
           ;; Numeric m-matrix column alignment state
           (nm-col-idx 0)
           (nm-state (matlab-ts-mode--ei-nm-matrix-state bol-pt))
           (nm-col-widths (nth 0 nm-state))
           (nm-first-col-extra (nth 1 nm-state))
           (nm-inside-matrix (nth 2 nm-state)))

      (when indent-has-tabs ;; Tabs in leading whitespace?  Tabs require eilb for untabify.
        (setq using-eilb (matlab--eilb-setup))
        (matlab-ts-mode--ei-insert-indent-level-spaces))

      (cl-loop
       while (and (< (point) eol-pt)
                  (< (treesit-node-end node) eol-pt))
       do
       (let* ((next-pair (progn
                           (goto-char (treesit-node-end node))
                           (or next2-pair
                               (matlab-ts-mode--ei-move-to-and-get-node-pair))))
              (next-node-type (or (car next-pair) (cl-return)))
              (next-node (cdr next-pair))
              (n-spaces-between next2-n-spaces-between))

         (setq next2-pair nil
               next2-n-spaces-between nil)

         (when matlab-ts-mode--indent-assert
           (setq orig-line-node-types
                 (matlab-ts-mode--ei-update-line-node-types orig-line-node-types node node-type)))

         (when (not n-spaces-between)
           ;; Fast path: compiled hash dispatch — O(1) for known type pairs.
           (setq n-spaces-between (gethash (cons node-type next-node-type)
                                           matlab-ts-mode--ei-spacing-fast))

           ;; Fast path: 3-node rule for power/transpose: val (^|.^) val -> 0 spaces.
           ;; In the original rule table, this rule appears before the generic wildcard rules
           ;; but AFTER the specific (number .^) rule.  We only override the 2-node result
           ;; when the 3-node preconditions are met: the specific (number .^) rule already
           ;; gives 1 space for "number .^" which is kept when next2 is not a val.
           (when (and (or (not n-spaces-between) (= n-spaces-between 1))
                      (or (string= node-type "identifier") (string= node-type "number"))
                      (or (string= next-node-type "^") (string= next-node-type ".^"))
                      ;; Rule (number .^) at line 176 precedes the 3-node rule and gives 1.
                      ;; Only check lookahead when the pair *could* be overridden.
                      (not (and (string= node-type "number") (string= next-node-type ".^"))))
             (save-excursion
               (goto-char (treesit-node-end next-node))
               (let* ((pair (matlab-ts-mode--ei-move-to-and-get-node-pair))
                      (next2-node-type (car pair)))
                 (when (and next2-node-type
                            (or (string= next2-node-type "identifier")
                                (string= next2-node-type "number")))
                   (setq next2-pair pair
                         next2-n-spaces-between 0
                         n-spaces-between 0)))))

           ;; Slow path fallback: linear scan (should be rare with a complete type list).
           (when (not n-spaces-between)
             (cl-loop for tuple in matlab-ts-mode--ei-spacing do
                      (let* ((node-re (nth 0 tuple))
                             (next-spec (nth 1 tuple))
                             (next-node-re (if (listp next-spec) (car next-spec) next-spec))
                             (next2-node-re (when (listp next-spec) (cdr next-spec))))

                        (when (and
                               (string-match-p node-re node-type)
                               (string-match-p next-node-re next-node-type)
                               (or (not next2-node-re)
                                   (save-excursion
                                     (goto-char (treesit-node-end next-node))
                                     (let* ((pair (matlab-ts-mode--ei-move-to-and-get-node-pair))
                                            (next2-node-type
                                             (or (car pair)
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
                          (cl-return))))))

         (when (not n-spaces-between)
           (matlab-ts-mode--assert-msg
            (format "ei, unhandled node <\"%s\" %S> and next-node <\"%s\" %S>"
                    node-type node next-node-type next-node)))

         ;; Numeric m-matrix column alignment: pad entries to their column widths
         ;; for right-alignment.  Track the column index by counting entry nodes
         ;; (those that are not separators or brackets).  When next-node is an
         ;; entry, add (col-width - entry-width) extra spaces before it.
         ;; Only track columns inside the matrix (after the "[" node).
         (when nm-col-widths
           (when (and (string= node-type "[")
                      (matlab-ts-mode--ei-is-multi-line-matrix (treesit-node-parent node)))
             (setq nm-inside-matrix t))
           (when nm-inside-matrix
             (when (not (string-match-p matlab-ts-mode--ei-matrix-syntax-nodes node-type))
               (setq nm-col-idx (1+ nm-col-idx)))
             (when (and n-spaces-between
                        (not (string-match-p matlab-ts-mode--ei-matrix-syntax-nodes
                                             next-node-type)))
               (let* ((next-col (1+ nm-col-idx))
                      (raw-width (nth (1- next-col) nm-col-widths))
                      (col-width (if (and (= next-col 1) nm-first-col-extra)
                                     (+ raw-width nm-first-col-extra)
                                   raw-width))
                      (entry-width (- (treesit-node-end next-node)
                                      (treesit-node-start next-node))))
                 (when (and col-width (< entry-width col-width))
                   (setq n-spaces-between (+ n-spaces-between
                                             (- col-width entry-width))))))))

         (let* ((node-end (treesit-node-end node))
                (next-node-start (treesit-node-start next-node))
                (extra-chars (matlab-ts-mode--ei-node-extra-chars node node-end next-node-start)))

           ;; When not yet using eilb, check if existing content matches what we'd produce.
           ;; Switch to eilb when a mismatch is found.
           (when (not using-eilb)
             (let* ((gap-str (buffer-substring-no-properties node-end next-node-start))
                    (gap-matches (and (if (listp extra-chars)
                                          nil ;; list extra-chars require eilb retroactive insert
                                        (string= extra-chars ""))
                                      n-spaces-between
                                      (= (length gap-str) n-spaces-between)
                                      (not (string-match-p "[^ ]" gap-str)))))
               (when (not gap-matches)
                 ;; Mismatch found: copy content from BOL to node-start into eilb and continue
                 (let ((prefix (buffer-substring-no-properties
                                bol-pt (treesit-node-start node))))
                   (matlab--eilb-setup)
                   (setq using-eilb t)
                   (with-current-buffer matlab--eilb
                     (insert prefix))))))

           (when (equal start-node node)
             (setq pt-offset (if using-eilb
                                 (+ (matlab--eilb-length) start-offset)
                               (+ (- (treesit-node-start node) bol-pt) start-offset))))

           (when using-eilb
             (matlab--eilb-add-node-text node extra-chars n-spaces-between)))

         (setq node next-node
               node-type next-node-type)
         ))

      (when node ;; last node in line
        (let ((extra-chars (matlab-ts-mode--ei-node-extra-chars
                            node (min (treesit-node-end node) eol-pt) eol-pt)))
          ;; Check last node: is there trailing content after the last node?
          (when (not using-eilb)
            (when (or (listp extra-chars) (not (string= extra-chars "")))
              ;; Trailing extra-chars require eilb
              (let ((prefix (buffer-substring-no-properties bol-pt (treesit-node-start node))))
                (matlab--eilb-setup)
                (setq using-eilb t)
                (with-current-buffer matlab--eilb
                  (insert prefix)))))

          (when (equal start-node node)
            (setq pt-offset (if using-eilb
                                (+ (matlab--eilb-length) start-offset)
                              (+ (- (treesit-node-start node) bol-pt) start-offset))))
          (when matlab-ts-mode--indent-assert
            (setq orig-line-node-types
                  (matlab-ts-mode--ei-update-line-node-types orig-line-node-types node node-type)))
          (when using-eilb
            (matlab--eilb-add-node-text node extra-chars))))

      (setq matlab-ts-mode--ei-line-nodes-loc nil)
      (list (if using-eilb
                (matlab--eilb-content)
              (buffer-substring-no-properties bol-pt eol-pt))
            pt-offset orig-line-node-types first-node-pair))))

;; KEY: pos-bol of the matrix start line. VALUE: first-col-extra integer
(defvar-local matrix-ts-mode--ei-m-matrix-first-col-extra-cache nil)

(defun matlab-ts-mode--ei-m-matrix-first-col-extra (matrix-id)
  "Get matrix first-col-extra for indent alignment.
MATRIX-ID is either a tree-sitter matrix node or a `pos-bol'
position (integer) of the matrix node's start line.  Result is 0 or
`matlab-ts-mode--array-indent-level' divided by 2.  Consider the
following where `matlab-ts-mode--array-indent-level' is 2.

This will return 1
  m1 = [
         1 2
         3 4
       ];
This will return 0
  m2 = [1 2
        3 4];"

  (let* ((mat-pos-bol (if (treesit-node-p matrix-id)
                          (save-excursion (goto-char (treesit-node-start matrix-id))
                                          (pos-bol))
                        matrix-id))
         (first-col-extra (when matrix-ts-mode--ei-m-matrix-first-col-extra-cache
                            (gethash mat-pos-bol
                                     matrix-ts-mode--ei-m-matrix-first-col-extra-cache))))
    (when (not first-col-extra)
      (save-excursion
        ;; Move to point after the matrix open, "["
        (if (treesit-node-p matrix-id)
            (progn (goto-char (treesit-node-start matrix-id))
                   (forward-char)) ;; step over the "["
          ;; Else matrix-id is pos-bol
          (goto-char matrix-id)
          ;; Move to point after matrix open, which requires finding it, consider:
          ;;  v([1,2;3,4]) = [ 101,   21
          ;;                  3001, 4001];
          (let* ((eol-pt (pos-eol))
                 (mat-open-pt (re-search-forward "\\[" eol-pt)) ;; point after the "["
                 (have-other-mat-open (save-excursion (re-search-forward "\\[" eol-pt t))))
            (while have-other-mat-open
              (let ((parent (treesit-node-parent (treesit-node-at (1- mat-open-pt)))))
                (if (and (string= (treesit-node-type parent) "matrix")
                         (not (matlab-ts-mode--ei-is-multi-line-matrix parent)))
                    (setq have-other-mat-open nil)
                  (setq mat-open-pt (re-search-forward "\\[" eol-pt)))))))

        (setq first-col-extra
              (if (and (re-search-forward "[^ \t]" (pos-eol) t)
                       (progn (backward-char)
                              ;; have element (not a comment or ellipsis)
                              (not (looking-at (rx (or "%" "..."))))))
                  0
                1))
        (when matrix-ts-mode--ei-m-matrix-first-col-extra-cache
          (puthash mat-pos-bol first-col-extra matrix-ts-mode--ei-m-matrix-first-col-extra-cache))
        ))
    first-col-extra))

(defvar-local matlab-ts-mode--ei-m-matrix-col-widths-cache nil)

(defun matlab-ts-mode--ei-m-matrix-col-widths (matrix first-col-extra &optional first-col-only)
  "Get multi-line MATRIX column widths adding in FIRST-COL-EXTRA to first column.
If optional FIRST-COL-ONLY is non-nil, then return only the width of the
first column in MATRIX.
Returns alist where each element in the alist is (COLUMN-NUM . WIDTH)"

  (let* ((start-linenum (line-number-at-pos (treesit-node-start matrix)))
         (column-widths (when matlab-ts-mode--ei-m-matrix-col-widths-cache
                          (gethash start-linenum matlab-ts-mode--ei-m-matrix-col-widths-cache))))
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

      (when matlab-ts-mode--ei-m-matrix-col-widths-cache
        (puthash start-linenum column-widths matlab-ts-mode--ei-m-matrix-col-widths-cache)))

    column-widths))

(defun matlab-ts-mode--ei-get-m-matrix-row-in-line (t-matrix-node)
  "Given point within a matrix assignment statement, return row node or nil.
Example:  v = [1 2; ...
               ...
               3 4];
when on the 2nd continuation only line, nil is returned otherwise a row node.
If tmp T-MATRIX-NODE is non-nil, we use that to locate the first row."
  (save-excursion
    (if t-matrix-node
        ;; [1, 2, 3;      // child 0 is the '[', child 1 is the first row
        ;;  4, 5, 6]
        ;; [ ...          // child 0 is the '[', child 1 is line_continuation
        ;;  1, 2, 3;
        ;;  4, 5, 6]
        (let ((child1 (treesit-node-child t-matrix-node 1)))
          (when (and (string= (treesit-node-type child1) "row")
                     (= (line-number-at-pos (treesit-node-start t-matrix-node))
                        (line-number-at-pos (treesit-node-start child1))))
            ;; row-now on same line as use-matrix-node
            child1))
      ;; else on 2nd or later line, find the row
      (matlab-ts-mode--ei-fast-back-to-indentation)
      (when (looking-at "[^ \t\n\r]")
        (let (row-node
              found-ans)
          (cl-loop
           while (not found-ans) do

           (let* ((node-at-pt (treesit-node-at (point) 'matlab))
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
          row-node)))))

(defun matlab-ts-mode--ei-indent-matrix-in-tmp-buf (assign-node
                                                    t-buf-start-pt-linenum start-pt-offset)
  "Insert ASSIGN-NODE in to current tmp-buf and indent.
T-BUF-START-PT-LINENUM and START-PT-OFFSET are used to place the point
prior to electric indent without alignment in the tmp buf.

Point is left at beginning of line containing the ASSIGN-NODE text.
Returns the line number after the ASSIGN-NODE in the tmp-buf."
  (let (assign-str
        assign-end-linenum
        t-buf-start-pt-offset
        n-levels
        (n-extra-levels 0)
        (is-prop (treesit-parent-until assign-node (rx bos (or "properties" "arguments") eos))))

    (setq matlab-ts-mode--ei-tmp-buf-indent t)

    (with-current-buffer (treesit-node-buffer assign-node)
      (let* ((assign-start-pos (save-excursion (goto-char (treesit-node-start assign-node))
                                               (pos-bol)))
             (assign-end-pos (save-excursion (goto-char (treesit-node-end assign-node))
                                             (pos-eol)))
             (indent-spaces (- (treesit-node-start assign-node) assign-start-pos)))
        (setq assign-str (buffer-substring-no-properties assign-start-pos assign-end-pos)
              n-levels (if (= (mod indent-spaces matlab-ts-mode--indent-level) 0)
                           (/ indent-spaces matlab-ts-mode--indent-level)
                         ;; else: not at a standard level so no need to add conditionals as the
                         ;; indent level will be corrected later.
                         0))))
    (when is-prop
      (setq n-extra-levels (max (- n-levels 2) 0)
            n-levels 2))

    (if is-prop
        (insert "classdef foo\n" "properties\n")
      (cl-loop for level from 1 to n-levels do
               (insert "if 1\n")))

    (insert assign-str "\n")

    (setq assign-end-linenum (line-number-at-pos))

    (cl-loop for level from 1 to n-levels do
             (insert "end\n"))

    (matlab-ts-mode)

    ;; Indent to adjust spacing among operators, but don't do other alignment items
    (let ((matlab-ts-mode--ei-align-enabled nil)
          ;; t-utils-test-indent captures messages using treesit--indent-verbose and we don't
          ;; want to capture the messages from this temp indent-region.
          (treesit--indent-verbose nil))
      (matlab--eilb-setup)
      (when t-buf-start-pt-linenum
        (goto-char (point-min))
        (forward-line (1- t-buf-start-pt-linenum))
        (move-to-column start-pt-offset))

      (matlab-ts-mode--ei-indent-region (point-min) (point-max))

      (when t-buf-start-pt-linenum
        (setq t-buf-start-pt-offset (matlab--ei-offset))))

    (goto-char (point-min))
    (forward-line n-levels) ;; Leave point at assignment start
    (when (> n-extra-levels 0) ;; Add additional whitespace on left?
      (save-excursion
        (let ((start-point (point))
              (end-point (progn
                           (goto-char (point-min))
                           (forward-line assign-end-linenum))))
          (string-rectangle start-point end-point (make-string (* n-extra-levels 4) ? )))))
    (cons assign-end-linenum t-buf-start-pt-offset)))

;; This is used to cache matrix alignments for indent-region
;; It will be non-nil when called from indent-region.
(defvar-local matlab-ts-mode--ei-align-matrix-cache nil)

(cl-defun matlab-ts-mode--ei-align-line-in-m-matrix (assign-node
                                                     ei-info
                                                     &optional start-pt-linenum start-pt-offset)
  "Align current line with EI-INFO in a multi-line matrix of ASSIGN-NODE.
See `matlab-ts-mode--ei-get-new-line' for EI-INFO contents.  Optional
START-PT-LINENUM and START-PT-OFFSET give point prior to electric indent
region.  START-PT-LINENUM may be different from current line."
  (let* ((matrix-cache matlab-ts-mode--ei-align-matrix-cache) ;; non-nil if indenting a region
         (assign-start-linenum (line-number-at-pos (treesit-node-start assign-node)))
         (t-buf-start-pt-linenum (when start-pt-linenum
                                   (1+ (- start-pt-linenum assign-start-linenum))))
         (t-buf-ei-linenum (1+ (- (line-number-at-pos) assign-start-linenum)))
         (t-buf-row-linenum (if matlab-ts-mode--ei-align-matrix-cache 1 t-buf-ei-linenum)))
    (with-temp-buffer
      (unwind-protect
          (progn
            (let* (t-curr-linenum
                   (tmp-buf-info (matlab-ts-mode--ei-indent-matrix-in-tmp-buf
                                  assign-node t-buf-start-pt-linenum start-pt-offset))
                   (t-end-linenum (car tmp-buf-info))
                   (t-buf-start-pt-offset (cdr tmp-buf-info))
                   (t-assign-node (save-excursion
                                    (matlab-ts-mode--ei-fast-back-to-indentation)
                                    (treesit-parent-until (treesit-node-at (point) 'matlab)
                                                          "assignment")))
                   (t-matrix-node (if t-assign-node
                                      (treesit-node-child-by-field-name t-assign-node "right")
                                    (treesit-node-parent (treesit-search-subtree ;; else a property
                                                          (treesit-buffer-root-node)
                                                          (rx bos "[" eos) nil t))))
                   (t-matrix-node-linenum (line-number-at-pos (treesit-node-start t-matrix-node)))
                   (first-col-extra (matlab-ts-mode--ei-m-matrix-first-col-extra t-matrix-node))
                   (col-widths (matlab-ts-mode--ei-m-matrix-col-widths t-matrix-node
                                                                       first-col-extra)))
              ;; Move to the line of interest when directly indenting a line rather than a region.
              (when (and (not matrix-cache)
                         (> t-buf-ei-linenum 1))
                (forward-line (1- t-buf-ei-linenum)))

              ;; Adjust column widths
              (while (< (setq t-curr-linenum (line-number-at-pos)) t-end-linenum)
                (matlab-ts-mode--ei-fast-back-to-indentation)
                (let* ((row-node (when (>= t-curr-linenum t-matrix-node-linenum)
                                   (matlab-ts-mode--ei-get-m-matrix-row-in-line
                                    (when (= t-curr-linenum t-matrix-node-linenum) t-matrix-node))))
                       (indent-offset (when (looking-at "[^ \t\n\r]+") (matlab--ei-offset)))
                       ei-line
                       pt-offset)
                  (matlab--eilb-setup)
                  (if (and row-node indent-offset)
                      (let* ((col-num (length col-widths)) ;; Iterate last col down to first col
                             (indent-start-pt (point))
                             (r-content (buffer-substring-no-properties indent-start-pt (pos-eol)))
                             (matrix-offset (save-excursion
                                              (goto-char (treesit-node-start t-matrix-node))
                                              (1+ (- (point) (pos-bol))))))
                        (setq pt-offset (if start-pt-linenum
                                            (when (= t-buf-start-pt-linenum t-curr-linenum)
                                              t-buf-start-pt-offset)
                                          (nth 1 ei-info)))

                        ;; Adjust matrix row in eilb using r-content w/o leading indent-level spaces
                        (with-current-buffer matlab--eilb
                          (insert r-content))
                        (when (< matrix-offset indent-offset)
                          (when pt-offset
                            (setq pt-offset (- pt-offset (- indent-offset matrix-offset))))
                          (setq indent-offset matrix-offset))

                        (dolist (row-el (reverse (treesit-node-children row-node)))
                          (when (not (string-match-p ;; at a column?
                                      (rx bos (or "," "line_continuation" "comment") eos)
                                      (treesit-node-type row-el)))
                            (let ((width (or (alist-get col-num col-widths)
                                             (matlab-ts-mode--assert-msg "no col width")))
                                  (curr-width (- (treesit-node-end row-el)
                                                 (treesit-node-start row-el))))
                              (when (< curr-width width)
                                (let ((offset (- (treesit-node-start row-el) indent-start-pt))
                                      (n-spaces (- width curr-width)))
                                  (when (and pt-offset
                                             (or (= indent-start-pt (point-min))
                                                 (<= (+ offset matrix-offset) pt-offset)))
                                    (setq pt-offset (+ pt-offset n-spaces)))
                                  (with-current-buffer matlab--eilb
                                    ;; left align strings, right align all other matrix elements
                                    (if (string= (treesit-node-type row-el) "string")
                                        (goto-char (+ 1 offset (length
                                                                (treesit-node-text row-el))))
                                      (goto-char (1+ offset)))
                                    (insert (make-string n-spaces ? ))
                                    (goto-char (point-max)))))

                              (setq col-num (1- col-num)))))

                        (with-current-buffer matlab--eilb
                          (goto-char (point-min))
                          (insert (make-string indent-offset ? ))
                          (goto-char (point-max)))

                        (setq ei-line (matlab--eilb-content))
                        (when (= t-buf-row-linenum t-buf-ei-linenum)
                          (setq ei-info (list ei-line pt-offset (nth 2 ei-info) (nth 3 ei-info)))))

                    ;; Else a blank or continuation line w/o matrix content
                    (setq ei-line (buffer-substring-no-properties (pos-bol) (pos-eol))))

                  (when matrix-cache
                    (let* ((buf-linenum (1- (+ assign-start-linenum t-buf-row-linenum))))
                      (puthash buf-linenum (list ei-line pt-offset) matrix-cache)))

                  (if (not matrix-cache)
                      (goto-char (point-max))
                    (forward-line)
                    (setq t-buf-row-linenum (1+ t-buf-row-linenum)))))))
        ;; unwind forms
        (matlab--eilb-kill)))
    )
  ;; ei-info for current line
  ei-info)

(cl-defun matlab-ts-mode--ei-matrix-ends-on-line (matrix)
  "Does MATRIX end on a line by itself?"
  (save-excursion
    (goto-char (treesit-node-end matrix))
    (while (re-search-forward "[^ \t]" (pos-eol) t)
      (backward-char)
      ;; xxx looking at "," ";", "%" should be sufficient?
      (let ((node (treesit-node-at (point) 'matlab)))
        ;; xxx remove line_continuation
        (when (not (string-match-p (rx bos (or "," ";" "comment" "line_continuation") eos)
                                   (treesit-node-type node)))
          (cl-return-from matlab-ts-mode--ei-matrix-ends-on-line))
        (goto-char (treesit-node-end node)))))
  t)

(defvar-local matlab-ts-mode--ei-is-m-matrix-cache nil) ;; cache

(cl-defun matlab-ts-mode--ei-is-m-matrix (matrix &optional check-for-indent-mode-minimal)
  "Is MATRIX node a multi-line matrix?
We define a a multi-line matrix has one row per line ignoring blank lines and
comments.  If optional, CHECK-FOR-INDENT-MODE-MINIMAL is non-nil, this we
check if matrix is in an %-indent-mode:minimal region and if so return
nil."
  (when (and check-for-indent-mode-minimal
             (matlab-ts-mode--ei-in-disabled-region (line-number-at-pos
                                                     (treesit-node-start matrix))))
    (cl-return-from matlab-ts-mode--ei-is-m-matrix))

  (let* ((start-linenum (line-number-at-pos (treesit-node-start matrix)))
         (cache-value (when matlab-ts-mode--ei-is-m-matrix-cache
                        (gethash start-linenum matlab-ts-mode--ei-is-m-matrix-cache))))
    (when cache-value ;; 0 or 1
      (cl-return-from matlab-ts-mode--ei-is-m-matrix (= cache-value 1)))

    (let* ((end-line (line-number-at-pos (treesit-node-end matrix)))
           (is-m-matrix (and
                         ;; Is candidate for a multi-line matrix we can align if more than one line
                         (> end-line start-linenum)
                         ;; AND the "]" ends on it's own line
                         (matlab-ts-mode--ei-matrix-ends-on-line matrix)
                         ;; AND has no inner matrices and no ERROR nodes
                         (let ((s-node (treesit-search-subtree matrix (rx bos (or "ERROR" "]") eos)
                                                               nil t)))
                           (= (treesit-node-end s-node) (treesit-node-end matrix)))))
           (n-rows 0)
           n-cols)

      (when is-m-matrix
        (cl-loop
         for child in (treesit-node-children matrix) do
         (let ((child-type (treesit-node-type child)))
           (cond
            ;; Case: row
            ((string= child-type "row")
             (let ((row-start-bol (save-excursion (goto-char (treesit-node-start child))
                                                  (pos-bol))))

               ;; Not an m-matrix when row is not on one line, i.e. start line != end line of row.
               (when (not (= row-start-bol (save-excursion (goto-char (treesit-node-end child))
                                                           (pos-bol))))
                 (setq is-m-matrix nil)
                 (cl-return))

               ;; Not an m-matrix when more than one row is on the line
               (let ((next-node (treesit-node-next-sibling child)))
                 (when (and (string= (treesit-node-type next-node) "row")
                            (= row-start-bol (save-excursion
                                               (goto-char (treesit-node-start next-node))
                                               (pos-bol))))
                   (setq is-m-matrix nil)
                   (cl-return)))

               (setq n-rows (1+ n-rows))

               ;; Get num cols in row. Not an m-matrix if num cols doesn't match prior row.
               (let ((n-cols-in-row 0))
                 (dolist (el (treesit-node-children child))
                   (when (not (string= (treesit-node-type el) ","))
                     (setq n-cols-in-row (1+ n-cols-in-row))))
                 (if (not n-cols)
                     (setq n-cols n-cols-in-row)
                   (when (not (= n-cols n-cols-in-row))
                     (setq is-m-matrix nil)
                     (cl-return)))))
             )
            ;; Case: unexpected matrix child node
            ((not (string-match-p (rx bos (or "[" "]" "comment" "line_continuation" "\n") eos)
                                  child-type))
             (matlab-ts-mode--assert-msg (format "unexpected matrix child %S" child)))))))

      ;; Multi-line matrix (m-matrix) with each row is on its own line?
      (let ((ans (and is-m-matrix (> n-rows 1) (>= n-cols 1))))
        (when matlab-ts-mode--ei-is-m-matrix-cache
          ;; Use 1 or 0 so we can differentiate between nil and not a multi-line matrix
          (puthash start-linenum (if ans 1 0) matlab-ts-mode--ei-is-m-matrix-cache))
        ans))))

(cl-defun matlab-ts-mode--ei-struct-ends-on-line (struct)
  "Does function_call STRUCT node end on a line?"
  (save-excursion
    (goto-char (treesit-node-end struct))
    (let ((pt-eol (pos-eol)))
      (while (re-search-forward "^[ \t]" pt-eol t)
        (backward-char)
        (if (looking-at "%") ;; comment after end?
            (goto-char pt-eol)
          (if (looking-at ";") ;; semicolon okay
              (forward-char)
            (cl-return-from matlab-ts-mode--ei-struct-ends-on-line)))))
    t))

(defun matlab-ts-mode--ei-struct-starts-on-first-line (struct)
  "Is function_call STRUCT node have opening paren on start line?"
  (let ((start-bol (save-excursion (goto-char (treesit-node-start struct)) (pos-bol)))
        paren-node)
    (cl-loop
     for child in (treesit-node-children struct) do
     (when (string= (treesit-node-type child) "(")
       (setq paren-node child)
       (cl-return)))
    (and paren-node
         (= start-bol (save-excursion (goto-char (treesit-node-start paren-node)) (pos-bol))))))

(defvar-local matlab-ts-mode--ei-is-m-struct-cache nil) ;; cache

(cl-defun matlab-ts-mode--ei-is-m-struct (struct)
  "Is function_call STRUCT node a multi-line struct that can be aligned?
If so return `(max-field-width . arguments-node), else nil."

  (let* ((start-bol (save-excursion (goto-char (treesit-node-start struct)) (pos-bol)))
         (cache-value (when matlab-ts-mode--ei-is-m-struct-cache
                        (gethash start-bol matlab-ts-mode--ei-is-m-struct-cache)))
         (max-field-width 0))

    (when cache-value
      (cl-return-from matlab-ts-mode--ei-is-m-struct (when (> (car cache-value) 0) cache-value)))

    (let* ((end-bol (save-excursion (goto-char (treesit-node-end struct)) (pos-bol)))
           arguments-node
           (is-m-struct (and
                         ;; Is candidate for a multi-line struct we can align if more than one line
                         (> end-bol start-bol)
                         ;; AND the opening "(" is on the same line as the struct
                         (matlab-ts-mode--ei-struct-starts-on-first-line struct)
                         ;; AND the closing ")" ends on it's own line
                         (matlab-ts-mode--ei-struct-ends-on-line struct)
                         ;; AND we have an arguments node struct(arg1, .....)
                         (setq arguments-node (treesit-search-subtree struct
                                                                      (rx bos "arguments" eos))))))
      (when is-m-struct
        (let (tracking-bol ;; iterate through arguments, tracking how many args on a line
              (n-args-on-tracking-line 0))
          (cl-loop
           for child in (treesit-node-children arguments-node) do
           (when (not (string-match-p (rx bos (or "," "line_continuation" eos))
                                      (treesit-node-type child)))
             ;; either field or value?
             (let ((arg-bol (save-excursion (goto-char (treesit-node-start child)) (pos-bol))))
               (if (and tracking-bol (= tracking-bol arg-bol))
                   (progn (setq n-args-on-tracking-line (1+ n-args-on-tracking-line))
                          (when (> n-args-on-tracking-line 2)
                            (setq is-m-struct nil)
                            (cl-return)))
                 ;; on new line
                 (when (= n-args-on-tracking-line 1) ;; 0 or 2 is good, 1 is bad
                   (setq is-m-struct nil)
                   (cl-return))
                 (when (not (string= (treesit-node-type child) "string"))
                   (setq is-m-struct nil) ;; first arg on line must be the struct field string
                   (cl-return))
                 (when (string-match-p "," (treesit-node-text child))
                   (setq is-m-struct nil) ;; we need the first comma to be end of the field string
                   (cl-return))
                 (let ((field-width (length (treesit-node-text child))))
                   (when (> field-width max-field-width)
                     (setq max-field-width field-width)))
                 (setq tracking-bol arg-bol
                       n-args-on-tracking-line 1)))))
          (when (and n-args-on-tracking-line (= n-args-on-tracking-line 1)) ;; 0 or 2 is good
            (setq is-m-struct nil))))

      (let ((ans (when (and is-m-struct (> max-field-width 0))
                   `(,max-field-width . ,arguments-node))))
        (when matlab-ts-mode--ei-is-m-struct-cache
          ;; When not an align-able multi-line struct, use (0 . nil) to indicate it as such
          (puthash start-bol (if ans ans '(0 . nil))
                   matlab-ts-mode--ei-is-m-struct-cache))
        ans))))

(cl-defun matlab-ts-mode--ei-align-line-in-m-struct (tuple ei-info)
  "Align multi-line struct.
TUPLE = (list struct-assign-node max-field-width arguments-node) where
See `matlab-ts-mode--ei-get-new-line' for EI-INFO."

  (let* ((ei-line (nth 0 ei-info))
         (struct-assign-node (nth 0 tuple))
         (max-field-width (nth 1 tuple))
         (assign-bol (save-excursion (goto-char (treesit-node-start struct-assign-node)) (pos-bol)))
         comma-offset
         new-comma-offset)

    (if (= (pos-bol) assign-bol)
        ;; On "var = struct(........" line?
        (let* ((eq-offset (string-match-p "=" ei-line))
               (open-paren-offset (string-match-p "(" ei-line eq-offset))
               (arg (when (string-match "\\([^ \t]+\\)" ei-line (1+ open-paren-offset))
                      (match-string 0 ei-line))))
          (when (or (not arg) (string-match-p (rx bos "...") arg)) ;; skip continuations
            (cl-return-from matlab-ts-mode--ei-align-line-in-m-struct ei-info))
          (setq comma-offset (1+ (string-match-p "," ei-line open-paren-offset))
                new-comma-offset (+ 1 open-paren-offset max-field-width)))
      ;; Else on later line
      (when (string-match-p (rx bos (0+ (or " " "\t")) "...") ei-line) ;; skip continuations
        (cl-return-from matlab-ts-mode--ei-align-line-in-m-struct ei-info))
      (setq comma-offset (string-match-p "," ei-line))
      (when (not comma-offset) ;; Ending ");" by itself on a line
        ;; TopTester: electric_indent_struct_in_prop2.m
        (cl-return-from matlab-ts-mode--ei-align-line-in-m-struct ei-info))
      (setq comma-offset (1+ comma-offset) ;; point after the ","
            new-comma-offset (+ (string-match-p "[^ \t]" ei-line) max-field-width))
      (let ((first-node-in-line (cdr (nth 3 ei-info)))
            (arguments-node (nth 2 tuple)))
        (when (not (equal (treesit-node-parent first-node-in-line) arguments-node))
          ;; TopTester: electric_indent_struct_with_multiline_field_values.m
          (cl-return-from matlab-ts-mode--ei-align-line-in-m-struct ei-info))))

    (let ((n-spaces-to-add (1+ (- new-comma-offset comma-offset))))
      (when (not (= n-spaces-to-add 0))
        (setq ei-line (concat (substring ei-line 0 comma-offset)
                              (make-string n-spaces-to-add ? )
                              (substring ei-line comma-offset)))
        (let ((pt-offset (nth 1 ei-info)))
          (when (and pt-offset (> pt-offset comma-offset))
            (setq pt-offset (+ n-spaces-to-add pt-offset)))
          (setq ei-info (list ei-line pt-offset (nth 2 ei-info) (nth 3 ei-info)))))))
  ei-info)

(defun matlab-ts-mode--ei-is-assign (first-node a-type &optional assign-node)
  "Is FIRST-NODE of line for an assignment that matches A-TYPE?
If caller knows FIRST-NODE is in an assignment, ASSIGN-NODE should be
supplied to avoid search ancestors for the assignment node.  Returns
assignment node when A-TYPE is met, else nil.  A-TYPE can be:
 - \\='single-line : varName = value            % on a single line, e.g. v1 = 1
 - \\='m-matrix    : varName = [100   2         % matrix on multiple lines
                                  3 400];
 - \\='m-struct    : varName = struct(...       % struct on multiple lines
                        \\='field1\\', value1, ...
                        \\='otherField2\\', value2);
Note, \\='m-struct returns (list assignment-node max-field-width arguments-node) or nil."
  (when (not assign-node)
    (setq assign-node (treesit-parent-until first-node (rx bos "assignment" eos))))

  (when assign-node
    (save-excursion
      (forward-line 0)
      (when (re-search-forward "=" (pos-eol) t)
        (backward-char)
        (let ((eq-node (treesit-node-at (point) 'matlab)))
          ;; First "=" must be an assignment (assumptions elsewhere require this).
          (when (and (equal (treesit-node-type eq-node) "=")
                     (if (string= (treesit-node-type assign-node) "assignment")
                         (equal (treesit-node-type (treesit-node-parent eq-node))
                                "assignment")
                       (equal (treesit-node-type (treesit-node-parent
                                                  (treesit-node-parent eq-node)))
                              "property")))
            (cond
             ;; Single-line assignment? Example: v1 = [1, 2];
             ((eq a-type 'single-line)
              (when (<= (treesit-node-end assign-node) (pos-eol))
                assign-node))

             ;; Multi-line matrix or struct assignment? Examples:
             ;;   m1 = [100   2
             ;;          3 400];
             ;;   s = struct('field1',      value1, ...
             ;;              'otherField2', value2);
             ((or (eq a-type 'm-matrix) (eq a-type 'm-struct))
              (let ((next-node (treesit-node-next-sibling eq-node)))
                (while (equal (treesit-node-type next-node) "line_continuation")
                  (setq next-node (treesit-node-next-sibling next-node)))
                (when next-node
                  (cond
                   ((eq a-type 'm-matrix)
                    (when (and (string= (treesit-node-type next-node) "matrix")
                               (matlab-ts-mode--ei-is-m-matrix next-node))
                      assign-node))
                   ((eq a-type 'm-struct)
                    (when (and (string= (treesit-node-type next-node) "function_call")
                               (string= (treesit-node-text (treesit-node-child-by-field-name
                                                            next-node "name"))
                                        "struct")
                               ;; TopTester: electric_indent_struct_on_next_line.m
                               (= (line-number-at-pos (treesit-node-start assign-node))
                                  (line-number-at-pos (treesit-node-start next-node))))
                      (let ((pair (matlab-ts-mode--ei-is-m-struct next-node))) ;; cdr => arguments
                        (when (and pair (> (car pair) 0)) ;; max-field-width > 0?
                          (list assign-node (car pair) (cdr pair))))))))))

             (t
              (matlab-ts-mode--assert-msg (format "bad a-type %S" a-type))))))))))

(defun matlab-ts-mode--ei-point-in-m-type (ei-info m-type)
  "Are we in an M-TYPE line?
1. \\='m-matrix M-TYPE: return the assignment node when in an
   m-matrix else nil.  We define an m-matrix to be an assignment where
   there's more than one line, each row is on the same line, with same
   number of columns.  Example:
     m = [100   2;
            3 400];
2. \\='m-struct M-TYPE: return the assignment node if m-struct else
   nil.  We define an m-matrix to be an assignment where there's more than
   one line, each row is on the same line, with same number of columns:
     m = [100   2;
            3 400];
3. \\='single-line M-TYPE: return assignment node when on a single line,
   else nil.
See `matlab-ts-mode--ei-get-new-line' for EI-INFO."
  (let* ((first-node-in-line (cdr (nth 3 ei-info)))
         (assign-node (treesit-parent-until first-node-in-line
                                            (rx bos (or "property" "assignment") eos))))
    (when assign-node
      (save-excursion
        (goto-char (treesit-node-start assign-node))
        (matlab-ts-mode--ei-fast-back-to-indentation)
        (let ((first-node (treesit-node-at (point) 'matlab)))
          (matlab-ts-mode--ei-is-assign first-node m-type assign-node))))))

(defun matlab-ts-mode--ei-assign-offset (ei-line)
  "Get the assignment offset from the indent-level in EI-LINE."
  (let* ((first-char-offset (or (string-match-p "[^ \t]" ei-line)
                                (matlab-ts-mode--assert-msg "no first char")))
         (offset (- (or (string-match-p "=" ei-line)
                        (matlab-ts-mode--assert-msg "no ="))
                    first-char-offset)))
    offset))

;; This is used to cache aligned assignments for indent-region
(defvar-local matlab-ts-mode--ei-align-assign-cache nil)

(defun matlab-ts-mode--ei-align-assignments (ei-info)
  "Update EI-INFO to align assignments.
See `matlab-ts-mode--ei-get-new-line' for EI-INFO contents."
  (let ((first-node-in-line (cdr (nth 3 ei-info)))
        (eat-comma matlab-ts-mode--ei-line-nodes-eat-comma))
    ;; We are calling `matlab-ts-mode--ei-get-new-line' and hence need to have our own hidden comma
    ;; handling state.
    (setq matlab-ts-mode--ei-line-nodes-eat-comma nil)
    (when (matlab-ts-mode--ei-is-assign first-node-in-line 'single-line)
      (let* ((ei-line (nth 0 ei-info))
             (line-assign-offset (matlab-ts-mode--ei-assign-offset ei-line))
             assign-offset
             assign-bols
             line-start-pt)

        (when (or (not matlab-ts-mode--ei-align-assign-cache)
                  (not (setq assign-offset (gethash (pos-bol)
                                                    matlab-ts-mode--ei-align-assign-cache))))
          (setq assign-offset line-assign-offset)
          (setq assign-bols `(,(pos-bol)))
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
                     (l-first-node (cdr (nth 3 l-info))))
                (if (and l-first-node
                         (matlab-ts-mode--ei-is-assign l-first-node 'single-line))
                    (let ((l-offset (matlab-ts-mode--ei-assign-offset (nth 0 l-info))))
                      (push (pos-bol) assign-bols)
                      (when (> l-offset assign-offset)
                        (setq assign-offset l-offset)))
                  (cl-return))))))
          (when matlab-ts-mode--ei-align-assign-cache
            (dolist (assign-bol assign-bols)
              (puthash assign-bol assign-offset matlab-ts-mode--ei-align-assign-cache))))

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
              (setq ei-info (list ei-line new-pt-offset (nth 2 ei-info) (nth 3 ei-info))))))))
    (setq matlab-ts-mode--ei-line-nodes-eat-comma eat-comma))
  ei-info)

(defun matlab-ts-mode--ei-get-prop-node (ei-info)
  "Return property node for first node in EI-INFO.
Returns nil if not a class property, enum field, or argument property
node that ends on same line and has items to align."
  (if-let ((first-node-in-line-pair (nth 3 ei-info)))
      (let* ((modified-node-type (car first-node-in-line-pair))
             (first-node (cdr first-node-in-line-pair))
             (prop-id-node (pcase modified-node-type
                             ((or "prop-id" "enum-id")
                              first-node)
                             ("property_name"
                              (treesit-node-parent first-node))))
             (prop-node (treesit-node-parent prop-id-node)))
        ;; skip multi-line nodes for alignment (properties / arguments can span multiple lines)
        (when (and prop-node
                   (= (line-number-at-pos (treesit-node-start prop-node))
                      (line-number-at-pos (treesit-node-end prop-node)))
                   (> (length (treesit-node-children prop-node)) 1))
          prop-id-node))))

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
      (matlab-ts-mode--assert-msg (format "no property in ei-line %s" ei-line)))
    (- (match-end 0) (match-beginning 0))))

;; This is used to cache aligned properties/arguments for indent-region.
(defvar-local matlab-ts-mode--ei-align-prop-cache nil)

(defun matlab-ts-mode--ei-align-properties (ei-info)
  "Align properties and arguments in EI-INFO.
See `matlab-ts-mode--ei-get-new-line' for EI-INFO contents."
  (when (matlab-ts-mode--ei-get-prop-node ei-info)
    (let ((ei-info-p-length (matlab-ts-mode--ei-prop-length ei-info))
          (p-length (when matlab-ts-mode--ei-align-prop-cache
                      (gethash (pos-bol) matlab-ts-mode--ei-align-prop-cache)))
          (eat-comma matlab-ts-mode--ei-line-nodes-eat-comma))

      (when (not p-length)
        (save-excursion
          (forward-line 0)
          (let* ((line-start-pt (point))
                 (line-bols `(,line-start-pt)))

            (setq p-length ei-info-p-length)

            ;; Look backwards and then forwards for properties/arguments
            (cl-loop
             for direction in '(-1 1) do

             ;; We are calling `matlab-ts-mode--ei-get-new-line' and hence need to have our own
             ;; hidden comma handling state.
             (setq matlab-ts-mode--ei-line-nodes-eat-comma nil)

             (goto-char line-start-pt)
             (cl-loop
              while (not (if (= direction -1) (bobp) (eobp))) do
              (forward-line direction)
              (let* ((l-ei-info (matlab-ts-mode--ei-get-new-line)))
                (if (matlab-ts-mode--ei-get-prop-node l-ei-info)
                    (let ((l-p-length (matlab-ts-mode--ei-prop-length l-ei-info)))
                      (when matlab-ts-mode--ei-align-prop-cache
                        (push (pos-bol) line-bols))
                      (when (> l-p-length p-length)
                        (setq p-length l-p-length)))
                  (cl-return)))))

            (when matlab-ts-mode--ei-align-prop-cache
              (dolist (line-bol line-bols)
                (puthash line-bol p-length matlab-ts-mode--ei-align-prop-cache))))))

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
              (setq ei-info (list ei-line new-pt-offset (nth 2 ei-info) (nth 3 ei-info)))))))
      (setq matlab-ts-mode--ei-line-nodes-eat-comma eat-comma)))
  ei-info)

(defun matlab-ts-mode--ei-trailing-comment-offset (ei-info)
  "Get trailing comment (scope . offset) from first node in line.
See `matlab-ts-mode--ei-get-new-line' for EI-INFO contents.
To simplify implementation, we require that the first \"%\" character in
the line be the start of the trailing comment.  Thus,
  s = \"foo % bar\" % comment
is not identified as having a trailing comment and
  s = \"foo bar\" %comment
is identified as having a trailing comment."
  (when ei-info
    (save-excursion
      (forward-line 0)
      (when (re-search-forward "%" (pos-eol) t)
        (let ((c-node (treesit-node-at (point) 'matlab)))
          (when (equal (treesit-node-type c-node) "comment")
            (if-let* ((first-node (cdr (nth 3 ei-info)))
                      (scope
                       (or
                        ;; Line of matrix/cell? Skip 1st line because its indent level differs.
                        ;; TODO - handle first line, e.g.
                        ;;   cell2 = { % comment 1
                        ;;            [], [], [], []; % comment 2
                        ;;            0, 0, 0, 0;     % comment 3
                        ;;           };
                        (let ((a-node (treesit-parent-until c-node
                                                            (rx bos (or "cell" "matrix") eos))))
                          (when (and a-node
                                     (not (= (pos-bol) (save-excursion
                                                         (goto-char (treesit-node-start a-node))
                                                         (pos-bol)))))
                            a-node))
                        ;; Use scope of first node in the line
                        (or (when (string-match-p (rx bol (or "arguments"
                                                              "end"
                                                              "enumeration"
                                                              "events"
                                                              "function"
                                                              "methods"
                                                              "properties")
                                                      eos)
                                                  (treesit-node-type first-node))
                              first-node)
                            (treesit-parent-until
                             first-node (rx bol (or "block"
                                                    "cell"
                                                    "matrix"
                                                    "source_file"
                                                    (seq (1+ anychar) "_clause")
                                                    (seq (1+ anychar) "_definition")
                                                    (seq (1+ anychar) "_definition")
                                                    (seq (1+ anychar) "_statement"))
                                            eol))))))
                ;; We align trailing comments when in same "scope" (same indent level):
                ;;    x   = [1, 2, 3]; % comment 1 (aligned)
                ;;    xyz = 2;         % comment 2 (aligned)
                ;;
                ;;    foo1 blah % comment (aligned)
                ;;    c = 1;    % comment (aligned)
                ;; We do when not in same "scope":
                ;;    fooBarGooToo = [something                                ...
                ;;                    otherThing(wIndex):theEndIndex(wIndex)-1}]; %#ok<AGROW>
                ;;    P0 = [P0 foo.blah{wIndex}.foobarGooOrig(:,1:end-1)]; %#ok<AGROW>
                ;;
                ;;    if a % comment1                  <= we shouldn't align this
                ;;       b = [1, 2, 3]; % comment2
                ;;       c = 1;         % comment3
                (let* ((new-line (nth 0 ei-info))
                       (indent-col (string-match-p "[^ \t]" new-line))
                       (offset (- (string-match-p "%" new-line) indent-col)))
                  (when (> offset 0)
                    ;; have trailing comment at offset from first char in new-line
                    `(,scope . ,offset))))))))))

;; This is used to cache comment alignments for indent-region
(defvar-local matlab-ts-mode--ei-align-comment-cache nil)

(defun matlab-ts-mode--ei-align-trailing-comments (ei-info)
  "Align trailing comments in EI-INFO.
See `matlab-ts-mode--ei-get-new-line' for EI-INFO contents."
  (if-let* ((line-comment-pair (matlab-ts-mode--ei-trailing-comment-offset ei-info)))
      (let ((eat-comma matlab-ts-mode--ei-line-nodes-eat-comma)
            comment-offset)

        ;; Compute desired comment-offset
        (when (or (not matlab-ts-mode--ei-align-comment-cache)
                  (not (setq comment-offset (gethash (pos-bol)
                                                     matlab-ts-mode--ei-align-comment-cache))))
          (let (line-bols
                line-start-pt
                (scope (car line-comment-pair)))

            (setq comment-offset (cdr line-comment-pair))

            (setq line-bols `(,(pos-bol)))
            (save-excursion
              (forward-line 0)
              (setq line-start-pt (point))

              ;; Look backwards and then forwards for lines with trailing comments
              (cl-loop
               for direction in '(-1 1) do
               (goto-char line-start-pt)

               ;; We are calling `matlab-ts-mode--ei-get-new-line' and hence need to have our own
               ;; hidden comma handling state.
               (setq matlab-ts-mode--ei-line-nodes-eat-comma nil)

               (cl-loop
                while (not (if (= direction -1) (bobp) (eobp))) do
                (forward-line direction)
                (let ((ei-l-info (matlab-ts-mode--ei-get-new-line))
                      l-offset-pair)
                  (setq ei-l-info (matlab-ts-mode--ei-align-assignments ei-l-info))
                  (setq ei-l-info (matlab-ts-mode--ei-align-properties ei-l-info))
                  (setq l-offset-pair (matlab-ts-mode--ei-trailing-comment-offset ei-l-info))
                  (if (and l-offset-pair (equal scope (car l-offset-pair)))
                      (let ((l-offset (cdr l-offset-pair)))
                        (push (pos-bol) line-bols)
                        (when (> l-offset comment-offset)
                          (setq comment-offset l-offset)))
                    (cl-return))))))

            (when matlab-ts-mode--ei-align-comment-cache
              (dolist (line-bol line-bols)
                (puthash line-bol comment-offset matlab-ts-mode--ei-align-comment-cache)))))

        (let ((diff (- comment-offset (cdr line-comment-pair))))
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
              (setq ei-info (list ei-line new-pt-offset (nth 2 ei-info) (nth 3 ei-info))))))

        (setq matlab-ts-mode--ei-line-nodes-eat-comma eat-comma)))
  ei-info)

(cl-defun matlab-ts-mode--ei-align (ei-info &optional start-pt-linenum start-pt-offset)
  "Align elements in EI-INFO.
See `matlab-ts-mode--ei-get-new-line' for EI-INFO contents.
Optional START-PT-LINENUM and START-PT-OFFSET give the point prior to electric
indent region."
  ;; Skip alignment for numeric-m-matrix lines because column alignment is handled in
  ;; `matlab-ts-mode--ei-get-new-line'.
  (let ((pos-bol-value (gethash (pos-bol) matlab-ts-mode--ei-m-matrix-pos-bol-map)))
    (when (and pos-bol-value
               (eq (car pos-bol-value) 'numeric-m-matrix))
      (cl-return-from matlab-ts-mode--ei-align ei-info)))

  (let ((matrix-assign-node (matlab-ts-mode--ei-point-in-m-type ei-info 'm-matrix)))
    (if matrix-assign-node
        (setq ei-info (matlab-ts-mode--ei-align-line-in-m-matrix matrix-assign-node ei-info
                                                                 start-pt-linenum start-pt-offset))
      (let ((tuple (matlab-ts-mode--ei-point-in-m-type ei-info 'm-struct)))
        (if tuple
            (setq ei-info (matlab-ts-mode--ei-align-line-in-m-struct tuple ei-info))
          ;; else do single-line alignments
          (setq ei-info (matlab-ts-mode--ei-align-assignments ei-info))
          (setq ei-info (matlab-ts-mode--ei-align-properties ei-info))
          (setq ei-info (matlab-ts-mode--ei-align-trailing-comments ei-info))))))
  ei-info)

(defun matlab-ts-mode--ei-get-start-info (start-pt-linenum start-pt-offset)
  "Get start node and start offset in line prior to electric indent.
If START-PT-LINENUM and START-PT-OFFSET are non-nil, the give point
prior to electric indent.

Returns (cons start-node start-offset) where
- start-node is non-nil if the point is at a node and start-node will
  be the modified node from `matlab-ts-mode--ei-get-node-to-use'
- start-offset is the offset of the point from the beginning of start-node.
Example where point is on the \"d\" in width:
  area = width * length;
           ^
start-node is the identifier node for width and start-offset is 2.
If start point is at end-of-line, (cons nil nil) is returned because we
need to just move the point to the end of the electric indented line."
  (let (start-node
        start-offset)
    (save-excursion
      (when start-pt-linenum
        (goto-char (point-min))
        (forward-line (1- start-pt-linenum))
        (move-to-column start-pt-offset))

      (when (not (looking-at "[ \t]*$")) ;; when not at EOL
        (let ((new-start-pt (save-excursion
                              (let ((start-pt (point)))
                                (forward-line 0)
                                (when (and (re-search-forward "[^ \t]" (pos-eol) t)
                                           (> (point) start-pt))
                                  (backward-char)
                                  (point))))))
          (when new-start-pt
            (goto-char new-start-pt)))
        (let ((node (car (matlab-ts-mode--ei-get-node-to-use (treesit-node-at (point) 'matlab)))))
          (when (not (and node
                          (>= (point) (treesit-node-start node))
                          (<= (point) (treesit-node-end node))))
            (when (re-search-forward "[^ \t]" (pos-eol) t)
              (backward-char)
              (setq node (car (matlab-ts-mode--ei-get-node-to-use
                               (treesit-node-at (point) 'matlab))))))

          (when (and node
                     (>= (point) (treesit-node-start node))
                     (<= (point) (treesit-node-end node)))
            (setq start-node node)
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
      (let ((node (treesit-node-at (point) 'matlab)))
        (when (equal (treesit-node-type node) "number")
          (when (and line-pt
                     (<= (point) line-pt))
            (setq line-pt (1+ line-pt)))
          (insert " ")
          (setq end (1+ end))))
      (forward-char 2)))
  (when line-pt
    (goto-char line-pt)))

(defvar-local matlab-ts-mode--ei-orig-line-node-types-cache nil)

(cl-defun matlab-ts-mode--ei-assert-nodes-types-match (curr-line-node-types
                                                       orig-line-node-types
                                                       linenum)
  "Validate CURR-LINE-NODE-TYPES eq ORIG-LINE-NODE-TYPES for LINENUM."

  ;; We insert comma's in arrays, so curr-line-node-types may be larger, thus ignore commas
  (setq curr-line-node-types (replace-regexp-in-string " , " " " curr-line-node-types))
  (setq orig-line-node-types (replace-regexp-in-string " , " " " orig-line-node-types))

  (when (not (string= curr-line-node-types orig-line-node-types))

    ;; See https://github.com/acristoffers/tree-sitter-matlab/issues/149
    (dolist (keyword '("events" "enumeration" "methods" "arguments"))
      (let ((orig-modified (replace-regexp-in-string (rx "identifier line_continuation" eos)
                                                     (concat keyword "-fcn line_continuation")
                                                     orig-line-node-types)))
        (when (string= curr-line-node-types orig-modified)
          (cl-return-from matlab-ts-mode--ei-assert-nodes-types-match))))

    (matlab-ts-mode--assert-msg (format "line-node-types mismatch
  new:  \"%s\"
  orig: \"%s\"
  at line %d in %s"
                                        curr-line-node-types
                                        orig-line-node-types
                                        linenum
                                        (buffer-name)))))

(defun matlab-ts-mode--ei-assert-line-nodes-match (start-linenum end-linenum)
  "Assert that original line node types match modified line node types.
We examine lines between START-LINENUM and END-LINENUM inclusive."

  (save-excursion
    (let ((beg (progn
                 (goto-char (point-min))
                 (forward-line (1- start-linenum))
                 (point)))
          (end (progn
                 (goto-char (point-min))
                 (forward-line end-linenum)
                 (point))))
      (setq matlab-ts-mode--ei-line-nodes (matlab-ts-mode--ei-line-nodes-in-region beg end)
            matlab-ts-mode--ei-line-nodes-loc matlab-ts-mode--ei-line-nodes)))

  (goto-char (point-min))
  (forward-line (1- start-linenum))
  (cl-loop
   for linenum from start-linenum to end-linenum
   do
   (let ((orig-line-node-types (when matlab-ts-mode--ei-orig-line-node-types-cache
                                 (gethash linenum
                                          matlab-ts-mode--ei-orig-line-node-types-cache))))
     (when orig-line-node-types ;; line was updated

       (matlab-ts-mode--ei-fast-back-to-indentation)
       (let (curr-line-node-types
             (eol-pt (pos-eol)))
         (cl-loop
          while (< (point) eol-pt)
          do
          (let* ((pair (matlab-ts-mode--ei-move-to-and-get-node-pair))
                 (node-type (or (car pair)
                                (cl-return)))
                 (node (cdr pair)))
            (setq curr-line-node-types
                  (matlab-ts-mode--ei-update-line-node-types curr-line-node-types
                                                             node node-type))
            (let ((node-end (treesit-node-end node)))
              (if (< node-end eol-pt)
                  (goto-char node-end)
                (goto-char eol-pt)))))

         (matlab-ts-mode--ei-assert-nodes-types-match curr-line-node-types
                                                      orig-line-node-types
                                                      linenum)))
     (forward-line)))

  (setq matlab-ts-mode--ei-line-nodes nil
        matlab-ts-mode--ei-line-nodes-loc nil))

(defvar-local matlab-ts-mode--ei-get-disabled-regions-cache nil)

(defun matlab-ts-mode--ei-get-disabled-regions ()
  "Return regions disabled by %-indent-mode=minimal comments.
Electric indent can be disabled then enabled using comments:
  %-indent-mode=minimal
    <code and comments>
  %-indent-mode=full
Code in the minimal region has the indent-level (spaces to the left)
modified as required.  The elements within the lines are not
modified.  For example,

  %-indent-mode=minimal
  if a >    1
  disp( \"a > 1\")
      end
  %-indent-mode=full

is indented to the following.  Notice that thew whitespace in line
elements are not modified.

  %-indent-mode=minimal
  if a >    1
      disp( \"a > 1\")
  end
  %-indent-mode=full

If we remove the %-indent-mode=* comments, indent produces:

  if a > 1
      disp(\"a > 1\")
  end

Returns:
    \\='((START-LINE1 . END-LINE1) (START-LINE2 . END-LINE2) ...))
where START-LINE1 corresponds to the first %-indent-mode=minimal comment,
END-LINE1 corresponds to the first %-indent-mode=full comment and so on."
  (or matlab-ts-mode--ei-get-disabled-regions-cache
      (let (result
            start-line)
        (save-excursion
          (save-restriction
            (goto-char (point-min))
            (while (re-search-forward (rx bol (0+ (or " " "\t"))
                                          (group (or "%-indent-mode=minimal" "%-indent-mode=full"))
                                          word-end)
                                      nil t)
              (let ((directive (match-string 1)))
                (pcase directive
                  ("%-indent-mode=minimal"
                   (when (not start-line)
                     (setq start-line (line-number-at-pos))))
                  ("%-indent-mode=full"
                   (when start-line
                     (push `(,start-line . ,(line-number-at-pos)) result)
                     (setq start-line nil)))
                  (_
                   (matlab-ts-mode--assert-msg (format "bad directive, %s" directive))))))
            (when start-line
              (push `(,start-line . ,(line-number-at-pos (point-max))) result))))
        ;; Always return non-nil for caching and (-1 . 0) will never be used.
        (or (reverse result) '((-1 . 0))))))

(defun matlab-ts-mode--ei-in-disabled-region (&optional linenum)
  "Is LINENUM in a disabled region?
LINENUM defaults to the current line.
Returns the region (START-LINE . END-LINE) if disabled, else nil."
  (when (not linenum)
    (setq linenum (line-number-at-pos)))
  (let ((disabled-regions (matlab-ts-mode--ei-get-disabled-regions)))
    (cl-loop for region in disabled-regions do
             (let ((region-start (car region))
                   (region-end (cdr region)))
               (when (and (>= linenum region-start)
                          (<= linenum region-end))
                 (cl-return region))))))

(defun matlab-ts-mode--ei-setup (beg end)
  "Setup for indent in region BEG to END."

  ;; Expand region to enable alignments, e.g. consecutive statements, matrix alignment
  (save-excursion
    ;; Move beg "up" based on consecutive lines
    (goto-char beg)
    (forward-line -1)
    (while (and (not (bobp))
                (looking-at (rx (0+ (or " " "\t")) (not (or " " "\t")))))
      (setq beg (point))
      (forward-line -1))

    ;; Move up to handle matrix with blank lines
    (when (not (bobp))
      (goto-char beg)
      (when (re-search-forward "[^ \t\n\r]" nil t)
        (backward-char)
        (let ((matrix-node (treesit-parent-until (treesit-node-at (point)) (rx bol "matrix" eol))))
          (when matrix-node
            (goto-char (treesit-node-start matrix-node))
            (setq beg (pos-bol))))))

    ;; Move end "down" based on consecutive lines
    (goto-char end)
    (forward-line -1)
    (while (and (not (eobp))
                (looking-at (rx (0+ (or " " "\t")) (not (or " " "\t")))))
      (forward-line)
      (setq end (point)))

    ;; Move down to handle matrix with blank lines
    (when (not (eobp))
      (goto-char end)
      (when (re-search-backward "[^ \t\n\r]" nil t)
        (let ((matrix-node (treesit-parent-until (treesit-node-at (point)) (rx bol "matrix" eol))))
          (when matrix-node
            (goto-char (treesit-node-end matrix-node))
            (forward-line)
            (setq end (point)))))))

  (setq matlab-ts-mode--ei-line-nodes-eat-comma nil)
  (setq matlab-ts-mode--ei-line-nodes (matlab-ts-mode--ei-line-nodes-in-region beg end)))

(defun matlab-ts-mode--ei-cleanup ()
  "Free memory used during indent."
  (setq matlab-ts-mode--ei-line-nodes nil
        matlab-ts-mode--ei-errors-map nil
        matlab-ts-mode--ei-m-matrix-pos-bol-map nil
        matlab-ts-mode--ei-m-matrix-node-map nil
        matlab-ts-mode--ei-bol2loc-map nil))

(cl-defun matlab-ts-mode--ei-indent-elements-in-line (&optional is-indent-region
                                                                start-pt-linenum start-pt-offset)
  "Indent current line by adjusting spacing around elements.

When IS-INDENT-REGION is t, we return tuple

  (list NEW-LINE UPDATED NEW-START-PT-OFFSET)

Optional START-PT-LINENUM and START-PT-OFFSET, is used when
IS-INDENT-REGION is t to compute NEW-START-PT-OFFSET for that line.

When IS-INDENT-REGION is nil, we update the line and restore the point
to it's logical location when the line is updated."

  (when (not is-indent-region)
    (matlab-ts-mode--ei-setup (pos-bol) (pos-eol)))

  (when (matlab-ts-mode--ei-in-disabled-region)
    (cl-return-from matlab-ts-mode--ei-indent-elements-in-line))

  (let* ((start-node-and-offset
          (when (or (not is-indent-region)
                    (and start-pt-linenum (= start-pt-linenum (line-number-at-pos))))
            (matlab-ts-mode--ei-get-start-info start-pt-linenum start-pt-offset)))
         (start-node (car start-node-and-offset)) ;; may be nil
         (orig-line (buffer-substring-no-properties (pos-bol) (pos-eol)))
         cached-ei-info
         ei-info
         result)

    (if (and matlab-ts-mode--ei-align-matrix-cache
             (setq cached-ei-info
                   (gethash (line-number-at-pos) matlab-ts-mode--ei-align-matrix-cache)))
        ;; TopTester: test-matlab-ts-mode-electric-indent-files/electric_indent_matrix_cols.m
        (setq ei-info cached-ei-info)
      (setq ei-info (matlab-ts-mode--ei-get-new-line start-node (cdr start-node-and-offset))))

    (if ei-info
        (setq result
              (progn
                (when (and (not cached-ei-info) matlab-ts-mode--ei-align-enabled)
                  (setq ei-info (matlab-ts-mode--ei-align ei-info
                                                          start-pt-linenum start-pt-offset)))
                (let* ((ei-line (or (nth 0 ei-info) orig-line))
                       (pt-offset (nth 1 ei-info)) ;; non-nil if start-offset is non-nil
                       (orig-line-node-types (nth 2 ei-info))
                       (updated (and ei-info (not (string= orig-line ei-line)))))
                  (when (and updated pt-offset (and start-node (looking-at "[ \t]*$")))
                    (setq pt-offset (length ei-line))) ;; at eol

                  (when (and updated
                             matlab-ts-mode--indent-assert)
                    (when matlab-ts-mode--ei-orig-line-node-types-cache
                      (puthash (line-number-at-pos) orig-line-node-types
                               matlab-ts-mode--ei-orig-line-node-types-cache))
                    (matlab-ts-mode--ei-assert-line-match ei-line orig-line))

                  (if is-indent-region
                      (list ei-line updated pt-offset) ;; result
                    ;; Else updated the line if needed (TAB on a line to electric indents it).
                    (when updated
                      (delete-region (pos-bol) (pos-eol))
                      (insert ei-line)
                      (when pt-offset
                        (goto-char (+ (pos-bol) pt-offset))))
                    nil ;; return nil for TAB indent
                    ))))
      ;; else nothing updated
      (when is-indent-region
        (setq result (list orig-line))))

    (when (not is-indent-region)
      (matlab-ts-mode--ei-cleanup)
      (matlab--eilb-kill))
    result))

(defun matlab-ts-mode--ei-move-to-loc (start-pt-linenum start-pt-offset)
  "Move to location START-PT-LINENUM at column START-PT-OFFSET."
  (goto-char (point-min))
  (forward-line (1- start-pt-linenum))
  (let ((eol-col (- (pos-eol) (pos-bol))))
    (when (> start-pt-offset eol-col) ;; be robust too big a start-pt-offset
      ;; TopTester: electric_indent_start_pt_offset.m
      (setq start-pt-offset eol-col)))
  (forward-char start-pt-offset))

(defun matlab-ts-mode--ei-set-region-caches (init &optional beg end)
  "Setup electric indent hash table caches.
Optional region BEG END must be provided when INIT is t.
If INIT is non-nil, set to initial value, otherwise set to nil."

  (if init
      (matlab-ts-mode--ei-setup beg end)
    (matlab-ts-mode--ei-cleanup))

  (setq
   matlab-ts-mode--ei-align-assign-cache             (when init
                                                       (make-hash-table :test 'eql))
   matlab-ts-mode--ei-align-prop-cache               (when init
                                                       (make-hash-table :test 'eql))
   matlab-ts-mode--ei-align-comment-cache            (when init
                                                       (make-hash-table :test 'eql))
   matlab-ts-mode--ei-m-matrix-col-widths-cache      (when init
                                                       (make-hash-table :test 'eql))
   matrix-ts-mode--ei-m-matrix-first-col-extra-cache (when init
                                                       (make-hash-table :test 'eql))
   matlab-ts-mode--ei-is-m-matrix-cache              (when init
                                                       (make-hash-table :test 'eql))
   matlab-ts-mode--ei-align-matrix-cache             (when init
                                                       (make-hash-table :test 'eql))
   matlab-ts-mode--ei-is-m-struct-cache              (when init
                                                       (make-hash-table :test 'eql))
   matlab-ts-mode--ei-orig-line-node-types-cache     (when init
                                                       (make-hash-table :test 'eql))
   matlab-ts-mode--ei-get-disabled-regions-cache     (when init
                                                       (matlab-ts-mode--ei-get-disabled-regions))))

(defun matlab-ts-mode--ei-indent-region-impl (new-content-buf
                                              beg end start-pt start-linenum end-linenum)
  "Implementation for `matlab-ts-mode--ei-indent-region'.
NEW-CONTENT-BUF is used to electric indent BEG to END region.
START-PT is our starting point for indent.
START-LINENUM and END-LINENUM correspond to the BEG and END points."
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

  (let* ((max-end-linenum (= end-linenum (line-number-at-pos (point-max))))
         (start-pt-linenum (line-number-at-pos start-pt))
         (start-pt-offset (matlab--ei-offset start-pt))
         region-updated
         (i-linenum start-linenum))

    (forward-line 0)
    (while (<= i-linenum end-linenum)
      (let ((line-ending (if (or max-end-linenum (< i-linenum end-linenum)) "\n" "")))
        (if (matlab-ts-mode--ei-in-disabled-region i-linenum)
            (let ((curr-line (buffer-substring-no-properties (pos-bol) (pos-eol))))
              (with-current-buffer new-content-buf
                (insert curr-line line-ending)))
          ;; else: electric indent the line
          (let* ((tuple (matlab-ts-mode--ei-indent-elements-in-line
                         'indent-region start-pt-linenum start-pt-offset))
                 (new-line (nth 0 tuple))
                 (line-updated (nth 1 tuple))
                 (new-start-pt-offset (nth 2 tuple)))
            (with-current-buffer new-content-buf
              (insert new-line line-ending))
            (when (= i-linenum start-pt-linenum)
              (if new-start-pt-offset
                  (setq start-pt-offset new-start-pt-offset)
                (when line-updated
                  (setq start-pt-offset (length new-line)))))
            (when line-updated
              (setq region-updated t)))))
      (forward-line)
      (setq i-linenum (1+ i-linenum)))

    (when region-updated
      (save-excursion
        (goto-char beg)
        (delete-region beg end)
        (insert (with-current-buffer new-content-buf (buffer-string)))
        (when matlab-ts-mode--indent-assert
          (matlab-ts-mode--ei-assert-line-nodes-match start-linenum end-linenum))
        ;; Restore END point accounting for electric indent changes
        (goto-char (point-min))
        (forward-line end-linenum)
        (setq end (point))))

    (matlab-ts-mode--ei-move-to-loc start-pt-linenum start-pt-offset)
    (treesit-indent-region beg end)
    (point)))

(defun matlab-ts-mode--ei-indent-region (beg end)
  "Indent BEG END region by adjusting spacing around elements.
If BEG is not at start of line, it is moved to start of the line.
If END is not at end of line, it is moved to end of the line.
This expansion of the region is done to simplify electric indent."
  (let ((new-content-buf (get-buffer-create
                          (generate-new-buffer-name " *temp-matlab-indent-region*"))))
    (unwind-protect
        (let* ((start-pt (point))
               (start-linenum (line-number-at-pos beg))
               (end-linenum (save-excursion (goto-char end)
                                            (- (line-number-at-pos)
                                               (if (= (point) (pos-bol)) 1 0))))
               (start-disabled (matlab-ts-mode--ei-in-disabled-region start-linenum))
               (end-disabled (matlab-ts-mode--ei-in-disabled-region end-linenum))
               end-pt)

          (if (and start-disabled (equal start-disabled end-disabled))
              (treesit-indent-region beg end) ;; adjust indent-level only
            ;; Else electric indent
            (save-excursion
              (save-restriction
                (widen)
                (matlab-ts-mode--ei-workaround-143 beg end) ;; may insert spaces on BEG to END lines

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

                (matlab-ts-mode--ei-set-region-caches t beg end)

                (setq end-pt (matlab-ts-mode--ei-indent-region-impl
                              new-content-buf beg end
                              start-pt start-linenum end-linenum))))
            (goto-char end-pt)))

      ;; unwind-protect cleanup:
      (matlab--eilb-kill)
      (matlab-ts-mode--ei-set-region-caches nil)
      (kill-buffer new-content-buf))))

(provide 'matlab-ts-mode--ei)
;;; matlab-ts-mode--ei.el ends here

;; LocalWords:  SPDX gmail treesit defcustom bos eos isstring defun eol eobp setq curr cdr xr progn
;; LocalWords:  listp alist dolist setf tmp buf utils linenum nums bobp pcase untabify SPC eilb prev
;; LocalWords:  linenums reindent bol fubar repeat:ans defmacro bn impl puthash caadr caar gethash
;; LocalWords:  ERROR's repeat:nil lang xyz cdar lparen rparen lbrack rbrack lbrace rbrace eql consp
;; LocalWords:  geq eqeq neq memq bols ridx rchild defconst FFs stmt lstart nreverse rw
;; LocalWords:  setcar setcdr anychar
