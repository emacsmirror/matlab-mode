;;; matlab-ts-mode.el --- MATLAB(R) Tree-Sitter Mode -*- lexical-binding: t -*-

;; Copyright 2025 Free Software Foundation, Inc.
;;
;; URL: https://github.com/mathworks/Emacs-MATLAB-Mode
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Author: John Ciolfi <john.ciolfi.32@gmail.com>
;; Created: Jul-7-2025
;; Keywords: MATLAB

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
;; Tree-sitter, https://tree-sitter.github.io/tree-sitter
;; based matlab mode: `matlab-ts-mode'
;; using https://github.com/acristoffers/tree-sitter-matlab
;;
;; Install tree-sitter-matlab by taking the matlab.EXT (EXT= .dll, .so, .dylib)
;; from the latest release of https://github.com/emacs-tree-sitter/tree-sitter-langs
;; and rename it to ~/.emacs.d/tree-sitter/libtree-sitter-matlab.EXT
;;

;;; Code:

(require 'treesit)
(require 'matlab-ts-mode--builtins)

;;; Customizations

(defgroup matlab-ts nil
  "MATLAB(R) tree-sitter mode."
  :prefix "matlab-ts-mode-"
  :group 'languages)

(defface matlab-ts-mode-pragma-face
  '((t :inherit font-lock-comment-face
       :bold t))
  "*Face to use for pragma %# lines.")

(defface matlab-ts-mode-string-delimiter-face
  '((t :inherit font-lock-string-face
       :bold t))
  "*Face to use for \\='single quote\\=' and \"double quote\" string delimiters.")

(defface matlab-ts-mode-comment-heading-face
  '((t :inherit font-lock-comment-face
       :overline t
       :bold t))
  "*Face for \"%% code section\" headings when NOT in matlab-sections-minor-mode.")

(defface matlab-ts-mode-comment-to-do-marker-face
  '((((class color) (background light))
     :inherit font-lock-comment-face
     :background "yellow"
     :weight bold)
    (((class color) (background dark))
     :inherit font-lock-comment-face
     :background "yellow4"
     :weight bold))
  ;; Note we split up the markers with spaces below so C-s when editing this file
  ;; doesn't find them.
  (concat "*Face use to highlight "
          "TO" "DO," " FIX" "ME," " and XX" "X markers ignoring case in comments.
Guidelines:
 - FIX" "ME" " and XX" "X markers should be fixed prior to committing
   code to a source repository.
 - TO" "DO markers can remain in code and be committed with the code to a
   source repository.  TO" "DO markers should reflect improvements are are
   not problems with the existing code."))

(defface matlab-ts-mode-operator-face
  '((((class color) (background light)) :foreground "navy")
    (((class color) (background dark))  :foreground "LightBlue")
    (t :inverse-video t
       :weight bold))
  "*Face for operators: *, /, +, -, etc.")

(defface matlab-ts-mode-command-arg-face
  '((t :inherit font-lock-string-face
       :slant italic))
  "*Face used to highlight command dual arguments.")

(defface matlab-ts-mode-system-command-face
  '((t :inherit font-lock-builtin-face
       :slant italic))
  "*Face used to higlight \"!\" system commands.")

(defface matlab-ts-mode-property-face
  '((t :inherit font-lock-property-name-face
       :slant italic))
  "*Face used on properties, enumerations, and events.")

(defface matlab-ts-mode-number-face
  '((t :inherit font-lock-constant-face))
  "*Face used for numbers.")

(defcustom matlab-ts-mode-font-lock-level 3
  "*Level of font lock, 1 for minimal syntax highlighting and 4 for maximum."
  ;; Setting to 4 to results in parse errors causing too much "red". See 'syntax-error
  ;; font-lock feature below.
  :type '(choice (const :tag "Minimal" 1)
		 (const :tag "Low" 2)
		 (const :tag "Standard" 3)
		 (const :tag "Standard plus parse errors" 4)))

(defcustom matlab-ts-mode-on-save-fixes
  '(matlab-ts-mode-on-save-fix-name)
  "List of function symbols which offer to fix *.m files on save.
During save these functions are called and will prompt to fix issues in
*.m files.  Each function gets no arguments, and returns nothing.  They
can move point, but it will be restored for them."
  :type '(repeat (choice :tag "Function: "
                         (matlab-ts-mode-on-save-fix-name))))

;;; Global variables used in multiple code ";;; sections"

(defvar matlab-ts-mode--comment-heading-re
  "^[ \t]*\\(%%\\(?:[ \t].+\\)?\\)$"
  "Regexp matching \"%% headings\" lines.")

;;; Utilities

(defun matlab-ts-mode--real-node-at-point ()
  "Return \\='(pt . node) where node is not a newline.
The MATLAB tree-sitter grammar includes a newline as a node.
If we are on a newline, this will backup to a real node.
The returned point, pt, reflects that location."
  (let* ((pt (point))
         (node-at-point (treesit-node-at pt))
         ;; Handle case at end of a start node, e.g. point just after properties:
         ;;    properties
         ;;              ^
         ;;        foo;
         ;;    end
         ;; Likewise for comments
         ;;    % comment
         ;;             ^
         (real-node (if (and (equal "\n" (treesit-node-type node-at-point))
                             (> pt 1))
                        (progn
                          (setq pt (1- pt))
                          (treesit-node-at pt))
                      node-at-point)))
    (cons pt real-node)))

;;; File encoding

(defun matlab-ts-mode--check-file-encoding ()
  "Check/set file encoding.
Error is signaled if contents are corrupt because non-utf8 printable
content can crash Emacs via the matlab tree-sitter parser."

  ;; MCR check. Version info is at start.
  ;; R2025a example: V2MCC8000MEC2000MCR2000<binary-data>
  (save-excursion
    (goto-char (point-min))
    (when (looking-at "^V[0-9A-Z]+MCR[0-9]+")
      (fundamental-mode)
      (user-error "Not activating matlab-ts-mode because this is MATLAB Compiler Runtime content")))

  ;; TODO should we check for utf-8 and error when non-utf8?
  ;;    (when (not (string-match "utf-8" (symbol-name buffer-file-coding-system)))
  ;;       (user-error "Buffer does not have utf-8 encoding"))
  ;; note we cannot
  ;;       (set-buffer-file-coding-system 'utf-8))
  ;; because this would modify it and modes shouldn't modify the buffer.

  (let ((bad-char-point (save-excursion
                          (goto-char (point-min))
                          (when (re-search-forward "[^[:print:][:space:]]" nil t)
                            (point)))))
    (when bad-char-point
      (fundamental-mode)
      (goto-char bad-char-point)
      (user-error "Buffer appears corrupt, non-printable utf8 character at point %d: %c"
                  bad-char-point (char-before)))))

;;; Syntax table

(defvar matlab-ts-mode--syntax-table
  (let ((st (make-syntax-table (standard-syntax-table))))
    ;; Comment Handling:
    ;; 1. Single line comments: % text (single char start),
    ;;                          note includes "%{ text"
    ;; 2. Multiline comments:   %{
    ;;                            lines
    ;;                          %}
    ;; 3. Ellipsis line continuations comments: "... optional text"
    ;;    are handled in `matlab-ts-mode--syntax-propertize'
    (modify-syntax-entry ?%  "< 13"  st)
    (modify-syntax-entry ?{  "(} 2c" st)
    (modify-syntax-entry ?}  "){ 4c" st)
    (modify-syntax-entry ?\n ">"     st)

    ;; String Handling:
    ;;   Single quoted string (character vector):    'text'
    ;;   Double-quoted string:                       "text"
    ;;   Transpose:           varname'
    ;;   Quoted quotes:       ' don''t '    or " this "" "
    ;;   Unterminated Char V: ' text
    (modify-syntax-entry ?'  "\"" st)
    (modify-syntax-entry ?\" "\"" st)

    ;; Words and Symbols:
    (modify-syntax-entry ?_  "_" st)

    ;; Punctuation:
    (modify-syntax-entry ?\\ "." st)
    (modify-syntax-entry ?\t " " st)
    (modify-syntax-entry ?+  "." st)
    (modify-syntax-entry ?-  "." st)
    (modify-syntax-entry ?*  "." st)
    (modify-syntax-entry ?/  "." st)
    (modify-syntax-entry ?=  "." st)
    (modify-syntax-entry ?<  "." st)
    (modify-syntax-entry ?>  "." st)
    (modify-syntax-entry ?&  "." st)
    (modify-syntax-entry ?|  "." st)

    ;; Parenthetical blocks:
    ;;   Note: these are in standard syntax table, repeated here for completeness.
    (modify-syntax-entry ?\(  "()" st)
    (modify-syntax-entry ?\)  ")(" st)
    (modify-syntax-entry ?\[  "(]" st)
    (modify-syntax-entry ?\]  ")[" st)
    (modify-syntax-entry ?{   "(}" st)
    (modify-syntax-entry ?}   "){" st)

    st)
  "The matlab-ts-mode syntax table.")

(defun matlab-ts-mode--put-char-category (pos category)
  "At character POS, put text proerty CATEGORY."
  (when (not (eobp))
    (put-text-property pos (1+ pos) 'category category)
    (put-text-property pos (1+ pos) 'mcm t)))

(defmacro matlab-ts-mode--syntax-symbol (symbol syntax doc)
  "Create a new SYMBOL with DOC used as a text property category with SYNTAX."
  (declare (indent defvar) (debug (sexp form sexp)) (doc-string 3))
  `(progn (defconst ,symbol ,syntax ,doc)
	  (put ',symbol 'syntax-table ,symbol)))

;; In the Syntax Table descriptors, "<" is for comments
(matlab-ts-mode--syntax-symbol matlab-ts-mode--ellipsis-syntax (string-to-syntax "<")
  "Syntax placed on ellipsis to treat them as comments.")

(defun matlab-ts-mode--syntax-propertize (&optional start end)
  "Scan region between START and END to add properties.
If region is not specified, scan the whole buffer.
This will mark ellipsis line continuation's
  ... optional text
as comments which is how they are treated by MATLAB."
  (save-match-data ;; avoid 'Syntax Checking transmuted the match-data'
    (save-excursion
      ;; Scan region, but always expand to beginning of line
      (goto-char (or start (point-min)))
      (beginning-of-line)

      ;; Edits can change what properties characters can have so remove ours and reapply
      (remove-text-properties (point) (save-excursion (goto-char (or end (point-max)))
						      (end-of-line) (point))
			      '(category nil mcm nil))

      ;; Tell Emacs that ellipsis (...) line continuations are comments.
      (while (and (not (>= (point) (or end (point-max)))) (not (eobp)))
        (if (treesit-search-forward-goto (treesit-node-at (point))
                                         (rx bos "line_continuation" eos)
                                         t) ;; goto start of: ... optional text
	    (matlab-ts-mode--put-char-category (point) 'matlab-ts-mode--ellipsis-syntax)
          (goto-char (point-max)))))))

;;; font-lock

(defvar matlab-ts-mode--keywords
  ;; Nodes like "if" are captured by their text because they are part of a bigger node that captures
  ;; them as such (and need more than just their text to define the node), but it doesn't make much
  ;; sense to create a node for the text "break", "continue", etc. because that would create two
  ;; nodes for the same purpose, where one is sufficient.  In other words, "break" like nodes are
  ;; captured as named nodes, not as unnamed ones, so you need to use their node names instead of
  ;; the "content".  See https://github.com/acristoffers/tree-sitter-matlab/issues/25
  ;;
  ;; Keywords are documented here https://www.mathworks.com/help/matlab/ref/iskeyword.html
  ;; Note, arguments, methods, properties are semi-keywords in that in the right location
  ;; the are keywords, otherwise in the wrong location they are variables, but tree-sitter
  ;; correctly handles them by letting us look for these as content of the nodes.
  '("arguments"
    (break_statement)
    "case"
    "catch"
    "classdef"
    (continue_statement)
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
    (return_statement)
    "set." ;; when used in a classdef method, e.g. function obj = set.propName(obj, val)
    "spmd"
    "switch"
    "try"
    "while")
  "The matlab-ts-mode font-lock keywords.")

(defvar matlab-ts-mode--operators
  ;; https://www.mathworks.com/help/matlab/matlab_prog/matlab-operators-and-special-characters.html
  '("+"         ;; Addition or Unary plus, +value
    "-"         ;; Subtraction or Unary minus, -value
    ".*"        ;; Element-wise multiplication
    "*"         ;; Matrix multiplication
    "./"        ;; Element-wise right division
    "/"         ;; Matrix right division
    ".\\"       ;; Element-wise left division
    "\\"        ;; Matrix left division (also known as backslash)
    ".^"        ;; Element-wise power
    "^"         ;; Matrix power
    ".'"        ;; Transpose
    "'"         ;; Complex conjugate transpose
    "=="        ;; Equal to
    "~="        ;; Not equal to
    ">"         ;; Greater than
    ">="        ;; Greater than or equal to
    "<"         ;; Less than
    "<=" 	;; Less than or equal to
    "&"         ;; Find logical AND
    "|"         ;; Find logical OR
    "&&"        ;; Find logical AND (with short-circuiting)
    "||"        ;; Find logical OR (with short-circuiting)
    "~"	        ;; Find logical NOT
    "@"         ;; Create anonymous functions and function handles, call superclass methods
    ;; "!"      ;; "!" is like an operator, but has command-dual like syntax, so handled elsewhere
    "?"         ;; Retrieve metaclass information for class name
    "~"         ;; Represent logical NOT, suppress specific input or output arguments.
    "="         ;; Variable creation and indexing assignment.
    "<" "&"     ;; Specify one or more superclasses in a class definition.
    ".?"        ;; Specify the fields of a name-value structure as the names of all
    ;;           ;   writable properties of the class.
    ;;           ;   However, this isn't parsed correctly by matlab tree-sitter, see
    ;;           ;   https://github.com/acristoffers/tree-sitter-matlab/issues/35
    )
  "The matlab-ts-mode font-lock operators.")

(defvar matlab-ts-mode--type-functions
  '("double"
    "single"
    "int8"
    "int16"
    "int32"
    "int64"
    "uint8"
    "uint16"
    "uint32"
    "uint64")
  "The matlab-ts-mode data type functions.")

(cl-defun matlab-ts-mode--get-doc-comment-candidate (comment-node)
  "Get candidate doc comment node or nil.
Return a comment node based on COMMENT-NODE if it is a candidate for a
help doc comment."

  ;; Backup over a copyright comment line
  (when (string-match-p "\\`[ \t]*%[ \t]*copyright\\b"
                        (treesit-node-text comment-node))
    (setq comment-node (treesit-node-prev-sibling comment-node))
    (when (not (string= "comment" (treesit-node-type comment-node)))
      (cl-return-from matlab-ts-mode--get-doc-comment-candidate)))

  (let ((prev-node (treesit-node-prev-sibling comment-node)))
    (when prev-node
      (while (string-match-p (rx bos "line_continuation" eos)
			     (treesit-node-type prev-node))
        (setq prev-node (treesit-node-prev-sibling prev-node)))
      (let ((prev-type (treesit-node-type prev-node)))
        ;; The true (t) cases. Note line continuation ellipsis are allowed.
        ;;    function foo          function foo(a)
        ;;    % doc comment         % doc comment
        ;;    end                   end
        (when (or (string-match-p (rx bos (or
                                           "function_arguments" ;; function input args?
                                           "superclasses")      ;; subclass
                                      eos)
                                  prev-type)
	          (and (string= prev-type "identifier")           ;; id could be a fcn or class id
                       (let ((prev-sibling (treesit-node-prev-sibling prev-node)))
                         (and prev-sibling
	                      (string-match-p
                               (rx bos
                                   (or "function"         ;; fcn wihtout in and out args
                                       "function_output"  ;; fcn w/out args and no in args
                                       "classdef")        ;; base class
                                   eos)
                               (treesit-node-type prev-sibling))))))
          comment-node)))))

(defun matlab-ts-mode--is-doc-comment (comment-node parent)
  "Is the COMMENT-NODE under PARENT a help doc comment.
In MATLAB,

  function out = myFunction
  % The documentation help comment for myFunction immediately follows the
  % function defintion.

      % code comments are preceded with a blank line
      out = 1;
  end

  function out = myFunction
  % The documentation help comment for myFunction immediately follows the
  % function defintion.

  % copyright at column 0 and preceded by blank liness after the help comment

      % code comments are preceded with a blank line
      out = 1;
  end

  function out = myFunctionWithoutHelp

      % code comments are preceded with a blank line
      out = 1;
  end

Similar behavior for classdef's."

  (setq comment-node (matlab-ts-mode--get-doc-comment-candidate comment-node))
  (when (and comment-node
             (string-match-p (rx bos (or "function_definition" "class_definition") eos)
                             (treesit-node-type parent)))
    (let ((definition-point (treesit-node-start parent)))
      (save-excursion
        (goto-char (treesit-node-start comment-node))
        (beginning-of-line)

        ;; result - is doc comment?
        (or (<= (point) definition-point) ;; at definition?
            (and (> (point) definition-point)
                 (not (re-search-backward "^[ \t]*$" definition-point t))))))))

(defun matlab-ts-mode--doc-comment-capture (comment-node override start end &rest _)
  "Fontify function/classdef documentation comments.
COMMENT-NODE is the tree-sitter node from a treesit-font-lock-rules rule
and OVERRIDE is from that rule.  START and END specify the region to be
fontified which could be smaller or larger than the COMMENT-NODE
start-point and end-point."
  (when (matlab-ts-mode--is-doc-comment comment-node (treesit-node-parent comment-node))
    (treesit-fontify-with-override
     (treesit-node-start comment-node) (treesit-node-end comment-node)
     font-lock-doc-face override start end)))

(defun matlab-ts-mode--comment-heading-capture (comment-node override start end &rest _)
  "Fontify the \"%% heading\" start line in COMMENT-NODE.
COMMENT-NODE is the tree-sitter node from a treesit-font-lock-rules rule
and OVERRIDE is from that rule.  START and END specify the region to be
fontified which could be smaller or larger than the COMMENT-NODE
start-point and end-point."
  (save-excursion
    (goto-char (treesit-node-start comment-node))
    (beginning-of-line)
    (when (looking-at matlab-ts-mode--comment-heading-re)
      (let ((heading-start (match-beginning 1))
            (heading-end (match-end 1)))
        (treesit-fontify-with-override heading-start heading-end
                                       'matlab-ts-mode-comment-heading-face
                                       override start end)))))

(defun matlab-ts-mode--comment-to-do-capture (comment-node override start end &rest _)
  "Fontify comment to do, fix me, and triple-x markers.
COMMENT-NODE is the tree-sitter comment node from a
treesit-font-lock-rules rule and OVERRIDE is from that rule.  START and
END specify the region to be fontified which could be smaller or larger
than the COMMENT-NODE start-point and end-point."
  (save-excursion
    (let ((comment-end (treesit-node-end comment-node)))
      (goto-char (treesit-node-start comment-node))
      (while (< (point) comment-end)
        ;; Note, the markers below have spaces in them so we don't find them when searching "C-s"
        ;; while editing this file.
        (if (re-search-forward (rx word-start (group (or (seq "to" "do")
                                                         (seq "fix" "me")
                                                         (seq "x" "xx")))
                                   word-end)
                               comment-end t)
            (let ((keyword-start (match-beginning 1))
                  (keyword-end (match-end 1)))
              (treesit-fontify-with-override keyword-start keyword-end
                                             'matlab-ts-mode-comment-to-do-marker-face
                                             override start end))
          (goto-char comment-end))))))

(defun matlab-ts-mode--namespace-builtins-capture (field-expression-node override start end &rest _)
  "Fontify foo.bar.goo when it is a builtin function.
FIELD-EXPRESSION-NODE is the tree-sitter comment node from a
treesit-font-lock-rules rule and OVERRIDE is from that rule.  START and
END specify the region to be fontified which could be smaller or larger
than the FILED-EXPRESSION-NODE start-point and end-point."
  (let ((children (treesit-node-children field-expression-node))
        (path "")) ;; The "path" of FIELD-EXPRESSION, e.g. foo.bar.goo
    (cl-loop for child in children do
             (let ((child-type (treesit-node-type child)))
               (cond
                ;; Case: (identifier) or "."
                ((or (string= child-type "identifier")
                     (string= child-type "."))
                 (setq path (concat path (treesit-node-text child))))
                ;; Case: (function_call)
                ((string= child-type "function_call")
                 (let ((fcn-name-node (treesit-node-child-by-field-name child "name")))
                   (setq path (concat path (treesit-node-text fcn-name-node)))))
                ;; Case: other - shouldn't be able to get here, but be safe.
                (t
                 (cl-return)))))
    (let ((builtin-type (gethash path matlab-ts-mode--builtins-ht)))
      (when builtin-type
        (let ((builtin-start (treesit-node-start field-expression-node))
              builtin-end
              prop-start
              prop-end)
          (if (eq builtin-type t)
              (setq builtin-end (+ builtin-start (length path)))
            ;; builtin-type is 'property or 'enumeration
            (when (not (string-match "\\`\\(.+\\.\\)\\([^.]+\\)\\'" path))
              (error "Assert: failed to parse path, %s" path))
            (setq builtin-end (+ builtin-start (- (length (match-string 1 path)) 2)))
            ;; We give the "." before the property or enum font-lock-delimiter-face, hence the +2.
            (setq prop-start (+ builtin-end 2))
            (setq prop-end (+ prop-start (length (match-string 2 path)))))

          (treesit-fontify-with-override builtin-start builtin-end 'font-lock-builtin-face
                                         override start end)
          (when prop-start
            (treesit-fontify-with-override prop-start prop-end 'matlab-ts-mode-property-face
                                           override start end)))))))

(defun matlab-ts-mode--is-fcn-name-value (identifier-node)
  "Return t if IDENTIFIER-NODE is a function name-value property."
  (let ((next-sibling (treesit-node-next-sibling identifier-node)))
    (and next-sibling
         (string= (treesit-node-text next-sibling) "="))))

(defun matlab-ts-mode--is-identifier-builtin (identifier-node)
  "Return t if IDENTIFIER-NODE is a function provided with MATLAB."
  (let ((parent (treesit-node-parent identifier-node)))
    ;; Consider
    ;;    foo.bar.disp.goo = 1;  % both foo and disp are builtin functions, but in this case fields
    ;;    foo.bar(1)             % +foo/bar.m - bar is a builtin, but foo.bar(1) is not.
    (when (and (not (equal (treesit-node-type parent) "field_expression"))
               (not (equal (treesit-node-type (treesit-node-parent parent)) "field_expression")))
      (let ((id (treesit-node-text identifier-node)))
        (gethash id matlab-ts-mode--builtins-ht)))))

(defun matlab-ts-mode--is-command-builtin (command-node)
  "Return t if COMMAND-NODE is a function provided with MATLAB."
  (let ((command (treesit-node-text command-node)))
    (gethash command matlab-ts-mode--builtins-ht)))

(defvar matlab-ts-mode--font-lock-settings
  (treesit-font-lock-rules

   ;; F-Rule: Comments and line continuation: ... optional text
   :language 'matlab
   :feature 'comment
   '(
     ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_comment_types.m
     (comment) @font-lock-comment-face
     ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_continuation.m
     ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_continuation_fcn.m
     ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_continuation_fcn_g2.m
     ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_continuation_multiArgFcn.m
     (line_continuation) @font-lock-comment-face)

   ;; F-Rule: special comments that override normal comment font
   :language 'matlab
   :feature 'comment-special
   :override t
   '(;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_pragma_in_fcn.m
     ((comment) @matlab-ts-mode-pragma-face
      (:match "\\`%#.+\\'" @matlab-ts-mode-pragma-face)) ;; %#pragma's
     ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_comment_heading.m
     ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_sections.m
     ((comment) @matlab-ts-mode--comment-heading-capture) ;; %% comment heading
     ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_comment_no_doc_help.m
     ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_comment_fcn.m
     (function_definition (comment) @matlab-ts-mode--doc-comment-capture) ;; doc help comments
     ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_MyClass.m
     (class_definition (comment) @matlab-ts-mode--doc-comment-capture)) ;; doc help comments

   ;; F-Rule: to do, fix me, triple-x marker comment keywords
   ;; See: test-matlab-ts-mode-font-lock-files/font_lock_comment_markers.m
   :language 'matlab
   :feature 'comment-marker
   :override t
   '(((comment) @matlab-ts-mode--comment-to-do-capture))

   ;; F-Rule: keywords: if, else, etc.
   ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_keywords.m
   ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_class_methods.m
   ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_keyword_spmd.m
   :language 'matlab
   :feature 'keyword
   `([,@matlab-ts-mode--keywords] @font-lock-keyword-face)

   ;; F-Rule: function/classdef and items definiting them, e.g. the function arguments
   :language 'matlab
   :feature 'definition
   '(
     ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_fcn_small_no_args.m
     (function_definition name: (identifier) @font-lock-function-name-face)
     ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_symPosDef.m
     (class_definition name: (identifier) @font-lock-function-name-face)
     ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_MySubClass.m
     ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_MySubSubClass.m
     (superclasses (property_name (identifier)) @font-lock-function-name-face)
     ;; Function inputs: functionName(in1, in2, in3)
     ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_fcn_small_in_args.m
     (function_arguments arguments:
                         (identifier)      @font-lock-variable-name-face
                         ("," (identifier) @font-lock-variable-name-face) :*)
     ;; Function single output argument: function out = functionName(in1, in2)
     ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_fcn_small_out_args.m
     (function_output (identifier) @font-lock-variable-name-face)
     ;; Function multiple output arguments: function [out1, out2] = functionName(in1, in2)
     (function_output (multioutput_variable (identifier) @font-lock-variable-name-face))
     ;; Fields of: arguments ... end , properties ... end
     ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_fcn_arguments.m
     ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_class_properties.m
     ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_class_MultiplePropBlocks.m
     ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_class_prop_access.m
     (property (validation_functions (identifier) @font-lock-builtin-face))
     (property name: (identifier) @matlab-ts-mode-property-face
               (identifier) @font-lock-type-face :?)
     (property name: (property_name (identifier) @matlab-ts-mode-property-face)
               (identifier) @font-lock-type-face :?)
     ;; Enumeration's
     ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_class_enum.m
     ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_enum_FlowRate.m
     (enum (identifier) @matlab-ts-mode-property-face)
     ;; Events block in classdef
     ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_class_events.m
     (events (identifier) @matlab-ts-mode-property-face)
     ;; Attributes of properties, methods
     ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_class_attributes.m
     (attribute (identifier) @font-lock-type-face "=" (identifier) @font-lock-builtin-face)
     (attribute (identifier) @font-lock-type-face))

   ;; F-Rule: Function Name = Value arguments
   ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_fcn_name_value_properties.m
   :language 'matlab
   :feature 'fcn-name-value
   '(((function_call (arguments (identifier) @matlab-ts-mode-property-face))
      (:pred matlab-ts-mode--is-fcn-name-value @matlab-ts-mode-property-face)))

   ;; F-Rule: variable
   ;; Could add font-lock-variable-name-face to variable uses.  Consider
   ;;     i1 = [1, 2];
   ;;     i2 = i1(1) + i3 + i4(i3);
   ;; we know i1 and i2 are varialbles from the (assignment left: (identifier))
   ;; However, we don't know if i3 or i4 are variables or functions because a function
   ;; can be called with no arguments, e.g. to call i3 function use i3 or i3(). i4 could
   ;; be a variable indexed by i1 or a function.
   ;;
   ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_variable.m
   :language 'matlab
   :feature 'variable
   '((assignment left: (identifier) @font-lock-variable-name-face)
     (multioutput_variable (identifier) @font-lock-variable-name-face)
     (global_operator (identifier) @font-lock-variable-name-face)
     (persistent_operator (identifier) @font-lock-variable-name-face)
     (for_statement (iterator (identifier) @font-lock-variable-name-face)))

   ;; F-Rule: command dual arugments
   :language 'matlab
   :feature 'command-arg
   '((command_argument) @matlab-ts-mode-command-arg-face)

   ;; F-Rule: system and command dual commands.
   ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_command.m
   :language 'matlab
   :feature 'command-name
   '(((command_name) @matlab-ts-mode-system-command-face
      ;; System command: ! ls *.m *.txt
      (:match "^!" @matlab-ts-mode-system-command-face))
     ;; Command-dual with at least one arg: myFunction arg
     ;; Commands with no args could be a variable or a function
     ((command (command_name) @font-lock-function-call-face (command_argument))))

   ;; F-Rule: strings "double quote" and 'single quote'
   ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_strings.m
   :language 'matlab
   :feature 'string
   '((string_content) @font-lock-string-face
     ((string_content) ["\"" "'"]) @matlab-ts-mode-string-delimiter-face
     (string ["\"" "'"] @matlab-ts-mode-string-delimiter-face)
     (escape_sequence) @font-lock-escape-face
     (formatting_sequence) @font-lock-escape-face)

   ;; F-rule: operators: *, /, +, -, etc.
   ;; Note, this rule must come after the string rule because single quote (') can be a transpose or
   ;; string delimiter.
   ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_operators.m
   :language 'matlab
   :feature 'operator
   `([,@matlab-ts-mode--operators] @matlab-ts-mode-operator-face)

   ;; F-Rule: Types, e.g. int32()
   :language 'matlab
   :feature 'type
   `((function_call name: (identifier)
                    @font-lock-type-face
                    (:match ,(rx-to-string `(seq bos
                                                 (or ,@matlab-ts-mode--type-functions)
                                                 eos)
                                           t)
                            @font-lock-type-face)))

   ;; F-Rule: Constant literal numbers, e.g. 1234, 12.34, 10e10
   ;; We could use this for items like true, false, pi, etc. See some of these numbers in:
   ;; https://www.mathworks.com/content/dam/mathworks/fact-sheet/matlab-basic-functions-reference.pdf
   ;; however, they do show up as builtins, which to me seems more accurate.
   ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_numbers.m
   :language 'matlab
   :feature 'number
   '((number) @matlab-ts-mode-number-face)

   ;; F-Rule: Brackets
   ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_brackets.m
   :language 'matlab
   :feature 'bracket
   '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

   ;; F-Rule: Delimiters, e.g. semicolon
   ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_delimiters.m
   :language 'matlab
   :feature 'delimiter
   '((["." "," ":" ";"]) @font-lock-delimiter-face)

   ;; F-Rule: factory items that come with MATLAB, Simulink, or add-on products
   ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_builtins.m
   :language 'matlab
   :feature 'builtins
   `(((identifier) @font-lock-builtin-face
      (:pred matlab-ts-mode--is-identifier-builtin @font-lock-builtin-face))
     ((command_name) @font-lock-builtin-face
      (:pred matlab-ts-mode--is-command-builtin @font-lock-builtin-face)))

   ;; F-Rule: namespaces (the +dir's, class methods, etc.)
   ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_namespaces.m
   :language 'matlab
   :feature 'namespace-builtins
   :override t
   `((field_expression) @matlab-ts-mode--namespace-builtins-capture)

   ;; F-Rule: Syntax errors
   ;; Some errors span too much of the buffer's text, so one will probably not want this
   ;; enabled. Perhaps, once https://github.com/acristoffers/tree-sitter-matlab/issues/36
   ;; is fixed we can enable this, though a lot more testing is needed.
   ;;
   ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_error.m
   ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_error_small.m
   ;; See: tests/test-matlab-ts-mode-font-lock-files/font_lock_error_missing_continuation.m
   :language 'matlab
   :feature 'syntax-error
   :override t
   '((ERROR) @font-lock-warning-face)
   )
  "The matlab-ts-mode font-lock settings.")


;;; Indent

;; TODO add indent standard here.

;; We discourage customizing the indentation rules. Having one-style of consistent indentation makes
;; reading others' code easier.
(defvar matlab-ts-mode--indent-level 4
  "Indentation level.")
(defvar matlab-ts-mode--switch-indent-level (/ matlab-ts-mode--indent-level 2)
  "Indentation level for switch-case statements.")
(defvar matlab-ts-mode--array-indent-level 2
  "Indentation level for elements in an array.")

(defun matlab-ts-mode--i-error-matcher (node _parent _bol &rest _)
  "Is NODE an ERROR node or is it under an ERROR node?"
  (let (in-error-node)
    (while (and node
                (not in-error-node))
      (if (equal "ERROR" (treesit-node-type node))
          (setq in-error-node t)
        (setq node (treesit-node-parent node))))
    in-error-node))

(defvar matlab-ts-mode--i-error-row-matcher-pair)

(defun matlab-ts-mode--i-error-row-matcher (_node _parent bol &rest _)
  "Is point within an ERROR node and can we determine indent?
If so, set `matlab-ts-mode--i-error-row-matcher-pair' to the anchor and
offset we should use and return non-nil.  BOL, beginning-of-line point,
is where we start looking for the error node."
  ;; 1) Given
  ;;            mat = [ [1, 2]; [3, 4];
  ;;        TAB>
  ;;    we have parse tree
  ;;        (ERROR (identifier) = [
  ;;         (row
  ;;          (matrix [
  ;;           (row (number) , (number))
  ;;           ]))
  ;;         ;
  ;;         (row
  ;;          (matrix [
  ;;           (row (number) , (number))
  ;;           ]))
  ;;         ;)
  ;;
  ;; 2) Now add ellipsis continuation:
  ;;            mat = [ [1, 2]; [3, 4]; ...
  ;;        TAB>
  ;; we'll have parse tree
  ;;       (source_file
  ;;        (ERROR (identifier) = [
  ;;         (row
  ;;          (matrix [
  ;;           (row (number) , (number))
  ;;           ]))
  ;;         ;
  ;;         (row
  ;;          (matrix [
  ;;           (row (number) , (number))
  ;;           ]))
  ;;         ;)
  ;;        (line_continuation))
  ;; Notice the line_continuation is not under the ERROR node, so we need to find that,
  ;; hence below we navigate over line_continuation's.
  ;;
  ;; See https://github.com/acristoffers/tree-sitter-matlab/issues/46

  (save-excursion
    (goto-char bol)

    ;; Move inside the error node if at an error node.
    (when (string= (treesit-node-type (treesit-node-at (point))) "ERROR")
      (re-search-backward "[^ \t\n\r]" nil t))

    ;; If on a "[" or "{" move back (we don't want to shift the current line)
    (when (string-match-p (rx bos (or "[" "{") eos) (treesit-node-type (treesit-node-at (point))))
      (re-search-backward "[^ \t\n\r]" nil t))

    ;; Walk over line_continuation, ",", ";" and identify the "check node" we should be looking
    ;; at.
    (let ((check-node (treesit-node-at (point))))
      (while (string-match-p (rx bos (or "line_continuation" "," ";") eos)
                             (treesit-node-type check-node))
        (goto-char (treesit-node-start check-node))
        (if (re-search-backward "[^ \t\n\r]" nil t)
            (setq check-node (treesit-node-at (point)))
          ;; at start of buffer
          (setq check-node nil)))

      (when check-node
        ;; Look to see if we are under an ERROR node
        (let ((error-node (treesit-node-parent check-node)))
          (while (and error-node
                      (not (string= (treesit-node-type error-node) "ERROR")))
            (setq error-node (treesit-node-parent error-node)))

          ;; In an error-node, see if this error is due to an incomplete cell or matrix,
          ;; if so return the start point of the row to anchor against.
          (when error-node
            (while (and check-node
                        (not (string-match-p (rx bos (or "row" "[" "{") eos)
                                             (treesit-node-type check-node))))
              (setq check-node (treesit-node-parent check-node)))

            (when check-node
              (cond
               ((string-match-p (rx bos (or "[" "{") eos) (treesit-node-type check-node))
                (setq matlab-ts-mode--i-error-row-matcher-pair
                      (cons (treesit-node-start check-node) 2)))
               ;; Case: looking at row?
               (t
                ;; Find first row to anchor against
                (let ((prev-sibling (treesit-node-prev-sibling check-node)))
                 (while prev-sibling
                   (when (string= (treesit-node-type prev-sibling) "row")
                     (setq check-node prev-sibling))
                   (setq prev-sibling (treesit-node-prev-sibling prev-sibling))))
                ;; In an ERROR node of a matrix or cell, return anchor
                (setq matlab-ts-mode--i-error-row-matcher-pair
                      (cons (treesit-node-start check-node) 0)))))))))))

(defun matlab-ts-mode--i-error-row-anchor (&rest _)
  "Return the anchor computed by `matlab-ts-mode--i-error-row-matcher'."
  (car matlab-ts-mode--i-error-row-matcher-pair))

(defun matlab-ts-mode--i-error-row-offset (&rest _)
  "Return the offset computed by `matlab-ts-mode--i-error-row-matcher'."
  (cdr matlab-ts-mode--i-error-row-matcher-pair))

(defun matlab-ts-mode--i-doc-block-comment-matcher (node parent bol &rest _)
  "Is NODE, PARENT within a function/classdef doc block comment \"%{ ... %}\"?
BOL, beginning-of-line point, is where we start looking for the error
node."
  (and (not node)
       (string= "comment" (treesit-node-type parent))
       (not (save-excursion (goto-char bol)
                            (looking-at "%")))
       (matlab-ts-mode--is-doc-comment parent (treesit-node-parent parent))))

(defun matlab-ts-mode--i-doc-comment-matcher (node parent _bol &rest _)
  "Is NODE, PARENT a function/classdef doc comment?"
  (or (and (string= "comment" (or (treesit-node-type node) ""))
           (matlab-ts-mode--is-doc-comment node parent))
      (and (not node)
           (string= "comment" (treesit-node-type parent))
           (matlab-ts-mode--is-doc-comment parent (treesit-node-parent parent)))))

(defun matlab-ts-mode--i-in-block-comment-matcher (node parent bol &rest _)
  "Is NODE, PARENT, BOL within a block \"{% ... %)\"?"
  (and (not node)
       (string= "comment" (treesit-node-type parent))
       (not (save-excursion (goto-char bol)
                            (looking-at "%")))))

(defun matlab-ts-mode--i-block-comment-end-matcher (node parent bol &rest _)
  "Is NODE, PARENT, BOL last line of \"{% ... %)\"?"
  (and (not node)
       (string= "comment" (treesit-node-type parent))
       (save-excursion (goto-char bol)
                       (looking-at "%"))))

;; `matlab-ts-mode--function-indent-level'
;;
;; It is recommended that all function statemements have terminating end statements.  In some cases
;; for compatibilty MATLAB doesn't require a function end statement.  When functions do not have an
;; end, we don't indent the body of the function per older MATLAB coding standard.  Example:
;;
;;     function a = fcn1    |    function a = fcn2
;;     a = 1;               |        a = 1;
;;                          |    end
;;
;; If a *.m file contains functions, and one of the functions is terminated with end, then every
;; function in the file must be terminated with end.  If a *.m file contains a function with one or
;; more nested functions, then every function in the file must be terminated with end.  If a script
;; contains one or more local functions, then every function in the file must be terminated with
;; end.
;;
;; When we enter `matlab-ts-mode', we examine the content and set the state of this variable.  If
;; matlab-ts--function-indent-level is nil, while editing, if functions become terminated with
;; ends, we set this to t.  We never go from t to nil because it's easy to get in a temporary nil
;; state during edits and we don't want to cause unexpected indentation behavior.")

(defvar-local matlab-ts-mode--function-indent-level 'unset
  "Function indent level is 0 or `matlab-ts-mode--indent-level'.")

(defun matlab-ts-mode--set-function-indent-level (&optional _node parent _bol &rest _)
  "Setup the function indent level for end vs end-less functions.
For optional _NODE, PARENT, and _BOL see `treesit-simple-indent-rules'."

  ;; - When matlab-ts-mode--function-indent-level is 'unset we set this to
  ;;     0 or matlab-ts-mode--indent-level
  ;;   based on the buffer content.
  ;; - Otherwise, matlab-ts-mode--function-indent-level is 0, we will "upgrade" it to
  ;;   matlab-ts-mode--indent-level if function end's appear.

  (let ((root (if (and parent
                       (string= (treesit-node-type parent) "function_definition"))
                  parent
                (treesit-buffer-root-node))))
    (if (treesit-search-subtree (treesit-buffer-root-node) "\\`ERROR\\'")
        ;; If we have syntax errors, assume that functions will have ends when entering
        ;; matlab-ts-mode, otherwise leave matlab-ts--function-indent-level unchanged.
        (when (equal matlab-ts-mode--function-indent-level 'unset)
          (setq-local matlab-ts-mode--function-indent-level matlab-ts-mode--indent-level))
      (let ((first-fcn (treesit-search-subtree root (rx bos "function_definition" eos))))
        (if (not first-fcn)
            ;; assume that if functions are added they will have ends
            (setq-local matlab-ts-mode--function-indent-level matlab-ts-mode--indent-level)
          (let ((have-end (string= (treesit-node-type (treesit-node-child first-fcn -1)) "end")))
            (if (equal matlab-ts-mode--function-indent-level 'unset)
                (setq-local matlab-ts-mode--function-indent-level
                            (if have-end
                                matlab-ts-mode--indent-level
                              0))
              (when have-end
                (setq-local matlab-ts-mode--function-indent-level matlab-ts-mode--indent-level))
              ))))))
  matlab-ts-mode--function-indent-level)

(defun matlab-ts-mode--set-function-indent-level-for-gp (node parent bol &rest _)
  "Set and return offset for parent of PARENT (grand-parent of NODE) at BOL."
  (matlab-ts-mode--set-function-indent-level node (treesit-node-parent parent) bol))

;; TODO - keep or rm matlab-ts-mode--do-functions-have-end (it's not currently used)
(defun matlab-ts-mode--do-functions-have-end ()
  "Are functions terminated with an end keyword?"
  ;; matlab-ts-mode--function-indent-level is setup when we enter matlab-ts-mode
  (cl-assert (not (eq matlab-ts-mode--function-indent-level 'unset)))
  ;; A 0 indicates functions are not terminated by an end keyword
  (not (= matlab-ts-mode--function-indent-level 0)))

(defvar matlab-ts--indent-debug-rule
  '((lambda (node parent bol)
      (message "-->N:%S P:%S BOL:%S GP:%S NPS:%S"
               node parent bol
               (treesit-node-parent parent)
               (treesit-node-prev-sibling node))
      nil)
    nil
    0))

(defvar matlab-ts-mode--indent-assert nil
  "Tests should set this to t to identify when we fail to find an indent rule.")

(defvar matlab-ts-mode--indent-assert-rule
  '((lambda (node parent bol)
      (when matlab-ts-mode--indent-assert
        (error "Assert no indent rule for: N:%S P:%S BOL:%S GP:%S NPS:%S BUF:%S"
               node parent bol
               (treesit-node-parent parent)
               (treesit-node-prev-sibling node)
               (buffer-name))))
    nil
    0))

(defun matlab-ts-mode--row-indent-level (node parent _bol &rest _)
  "Indent level for a NODE in PARENT cell or matrix."

  ;; first-entry is the 2nd element, i.e. child 1. Child 0 is is the "[" or "{".
  ;; first-entry could be "line_continuation" or "row"
  ;; "row" can also be empty, e.g. start-point == end-point as in
  ;;       C = [
  ;;             ...
  ;;   TAB>      1 ...
  ;;           ]
  (let* ((first-entry (treesit-node-child parent 1))
         first-start)
    (if (and (not (equal node first-entry)) ;; point is not on first row
             (equal (treesit-node-type first-entry) "row")
             (not (= (setq first-start (treesit-node-start first-entry))
                     (treesit-node-end first-entry))))
        ;;         c = [2, 3;
        ;;  TAB>        3, 4];
        (let ((first-column (save-excursion
                              (goto-char first-start)
                              (current-column)))
              (array-column (save-excursion
                              (goto-char (treesit-node-start parent))
                              (current-column))))
          (- first-column array-column))
      matlab-ts-mode--array-indent-level)))

(defvar matlab-ts-mode--i-error-switch-matcher-pair)

(defun matlab-ts-mode--i-error-switch-matcher (node _parent bol &rest _)
  "Tab on an empty line after switch, case, or otherwise?
NODE is nil in this case and BOL, beginning-of-line point, is where we
are indenting.  If so, set
`matlab-ts-mode--i-error-switch-matcher-pair'.

  Example:  switch fcn1(a)
      TAB>    ^

Similar for for case and otherwise statements."
  (when (not node)
    (save-excursion
      (goto-char bol)
      (re-search-backward "[^ \t\n\r]" nil t)

      (let* ((check-node (treesit-node-at (point)))
             (error-node (treesit-node-parent check-node)))
        (while (and error-node
                    (not (string= (treesit-node-type error-node) "ERROR")))
          (setq error-node (treesit-node-parent error-node)))

        ;; In an error-node, see if this error is due to an incomplete switch statement.
        (when error-node
          (let ((child-node (treesit-node-child error-node 0)))
            (when (and child-node
                       (string-match-p (rx bos (or "switch" "case" "otherwise") eos)
                                       (treesit-node-type child-node)))
              (setq matlab-ts-mode--i-error-switch-matcher-pair
                    (cons (treesit-node-start child-node)
                          matlab-ts-mode--switch-indent-level)))))))))

(defun matlab-ts-mode--i-error-switch-anchor (&rest _)
  "Return the anchor computed by `matlab-ts-mode--i-error-switch-matcher'."
  (car matlab-ts-mode--i-error-switch-matcher-pair))

(defun matlab-ts-mode--i-error-switch-offset (&rest _)
  "Return the offset computed by `matlab-ts-mode--i-error-switch-matcher'."
  (cdr matlab-ts-mode--i-error-switch-matcher-pair))

(defun matlab-ts-mode--i-cont-matcher (node _parent _bol &rest _)
  "Is current NODE part of a valid continuation?"
  (let ((prev-sibling (treesit-node-prev-sibling node)))
    (and prev-sibling
         (string= (treesit-node-type prev-sibling) "line_continuation"))))

(defun matlab-ts-mode--i-cont-offset (_node parent _bol &rest _)
  "Get the ellipsis continuation offset based on PARENT.
This is `matlab-ts-mode--indent-level' or 0 when in a cell or matrix
row."
  (let ((row-node (let ((n parent))
                    (while (and n
                                (not (string= (treesit-node-type n) "row")))
                      (setq n (treesit-node-parent n)))
                    n)))
    (if row-node
        0
      matlab-ts-mode--indent-level)))


(defvar matlab-ts-mode--i-cont-incomplete-matcher-pair)

(defun matlab-ts-mode--i-cont-incomplete-matcher (node parent _bol &rest _)
  "Is current line part of an ellipsis line continuation?
If so, set `matlab-ts-mode--i-cont-incomplete-matcher-pair'.  This is for
incomplete statements where NODE is nil and PARENT is line_continuation."

  ;; Case when code is incomplete:
  ;;
  ;;        x = 1 + myfcn(a, ...
  ;;   TAB>               ^       NODE is nil and PARENT is line_continuation

  (when (and (not node)
             (string= (treesit-node-type parent) "line_continuation"))
    (save-excursion
      (let ((check-node parent))
        ;; Walk over line_continuation and identify the "check node" we should be looking at.
        (while (string-match-p (rx bos "line_continuation" eos)
                               (treesit-node-type check-node))
          (goto-char (treesit-node-start check-node))
          (if (re-search-backward "[^ \t\n\r]" nil t)
              (setq check-node (treesit-node-at (point)))
            ;; at start of buffer
            (setq check-node nil)))

        (when check-node
          (let ((anchor-node check-node)
                paren-with-args)

            ;; Look for open paren of a function-call, index, or expression grouping
            (while (and anchor-node
                        (not (string-match-p (rx bos "(" eos)
                                             (treesit-node-type anchor-node))))
              (setq paren-with-args t)
              (setq anchor-node (treesit-node-prev-sibling anchor-node)))

            ;; If we found "( ..." then we anchor off parent
            (when (and anchor-node (not paren-with-args))
              (setq anchor-node nil))

            ;; Look for parent that we can use for indent
            (when (not anchor-node)
              (setq anchor-node check-node)
              (while (and anchor-node
                          (not (string-match-p (rx bos (or "ERROR" "arguments") eos)
                                               (treesit-node-type anchor-node))))
                (setq anchor-node (treesit-node-parent anchor-node))))

            (when anchor-node
              (setq matlab-ts-mode--i-cont-incomplete-matcher-pair
                    (cons (treesit-node-start anchor-node)
                          (pcase (treesit-node-type anchor-node)
                            ("(" 1)
                            ("arguments" 0)
                            ("ERROR" matlab-ts-mode--indent-level)
                            (_ (error "Assert - unhandled anchor-node, %S" anchor-node)))))
              )))))))

(defun matlab-ts-mode--i-cont-incomplete-anchor (&rest _)
  "Return the anchor computed by `matlab-ts-mode--i-cont-incomplete-matcher'."
  (car matlab-ts-mode--i-cont-incomplete-matcher-pair))

(defun matlab-ts-mode--i-cont-incomplete-offset (&rest _)
  "Return the offset computed by `matlab-ts-mode--i-cont-incomplete-matcher'."
  (cdr matlab-ts-mode--i-cont-incomplete-matcher-pair))

(defvar matlab-ts-mode--indent-rules
  `((matlab

     ;; I-Rule: syntax errors
     ;; See: tests/test-matlab-ts-mode-indent-files/indent_syntax_error1.m
     ;; See: tests/test-matlab-ts-mode-indent-files/indent_syntax_error2.m
     ;; See: tests/test-matlab-ts-mode-indent-xr-files/indent_xr_continuation1.m
     (,#'matlab-ts-mode--i-error-matcher no-indent 0)

     ;; I-Rule: cell/matrix row matcher when in an error node
     ;;         mat0 = [1, 2
     ;;                 ^            <-- TAB or RET on prior line goes here
     ;; See: tests/test-matlab-ts-mode-indent-xr-files/indent_xr_mat*.m
     (,#'matlab-ts-mode--i-error-row-matcher
      ,#'matlab-ts-mode--i-error-row-anchor
      ,#'matlab-ts-mode--i-error-row-offset)

     ;; I-Rule: classdef's, function's, or code for a script that is at the top-level
     ((parent-is ,(rx bos "source_file" eos)) column-0 0)

     ;; I-Rule: within a function/classdef doc block comment "%{ ... %}"?
     (,#'matlab-ts-mode--i-doc-block-comment-matcher parent 2)

     ;; I-Rule: function/classdef doc comment?
     (,#'matlab-ts-mode--i-doc-comment-matcher parent 0)

     ;; I-Rule: within a code block comment "%{ ... %}"?
     (,#'matlab-ts-mode--i-in-block-comment-matcher parent 2)

     ;; I-Rule: last line of code block coment "%{ ... %}"?
     (,#'matlab-ts-mode--i-block-comment-end-matcher parent 0)

     ;; I-Rule: switch case and otherwise statements
     ((node-is ,(rx bos (or "case_clause" "otherwise_clause") eos))
      parent ,matlab-ts-mode--switch-indent-level)

     ;; I-Rule: nested functions
     ((n-p-gp ,(rx bos "function_definition" eos)
              ,(rx bos "block" eos)
              ,(rx bos "function_definition" eos))
      parent 0)

     ;; I-Rule: elseif, else, catch, end statements go back to parent level
     ((node-is ,(rx bos (or "elseif_clause" "else_clause" "catch_clause" "end") eos)) parent 0)

     ;; I-Rule: first line of code witin a switch case or otherwise statement, node is block
     ((parent-is ,(rx bos (or "switch_statement" "case_clause" "otherwise_clause") eos))
      parent ,matlab-ts-mode--switch-indent-level)

     ;; I-Rule: function's
     ((parent-is ,(rx bos "function_definition" eos))
      parent ,#'matlab-ts-mode--set-function-indent-level-for-gp)

     ;; I-Rule: function a = indent_xr_fun()<RET>
     ;; See: tests/test-matlab-ts-mode-indent-xr-files/indent_xr_fun*.m
     ((n-p-gp nil ,(rx bos "\n" eos) ,(rx bos (or "function_definition") eos))
      grand-parent ,#'matlab-ts-mode--set-function-indent-level)


     ;; I-Rule: constructs within classdef or function's.
     ((node-is ,(rx bos (or "arguments_statement" "block" "enumeration" "enum" "methods" "events"
                            "function_definition" "property" "properties")
                    eos))
      parent ,matlab-ts-mode--indent-level)

     ;; I-Rule: items in blocks
     ((n-p-gp nil ,(rx bos "property" eos) ,(rx bos "properties" eos))
      grand-parent ,matlab-ts-mode--indent-level)

     ;; I-Rule: continuation of properties
     ((n-p-gp nil nil ,(rx bos "property" eos))
      grand-parent ,matlab-ts-mode--indent-level)

     ;; I-Rule: code in if, for, methods, arguments statements, etc.
     ((parent-is ,(rx bos (or "if_statement" "for_statement" "while_statement"
                              "methods" "events" "enumeration"
                              "function_definition" "arguments_statement"
                              "properties")
                      eos))
      parent ,matlab-ts-mode--indent-level)


     ;; I-Rule: case 10<RET>
     ((n-p-gp nil ,(rx bos "\n" eos) ,(rx bos (or "switch_statement" "case_clause"
                                                  "otherwise_clause")
                                          eos))
      grand-parent ,matlab-ts-mode--switch-indent-level)

     ;; I-Rule:  if condition1 || ...     |      if condition1 + condition2 == ...
     ;; <TAB>       condition2 || ...     |         2770000 ...
     ((parent-is ,(rx bos (or "boolean_operator" "comparison_operator") eos)) parent 0)

     ;; I-Rule:  elseif ...
     ;; <TAB>        condition2 || ...
     ((parent-is ,(rx bos (or "else_clause" "elseif_clause") eos))
      parent ,matlab-ts-mode--indent-level)

     ;; I-Rule: if variable ...
     ;;             ^             <== TAB or RET on prior line
     ;;         end
     ;; See: tests/test-matlab-ts-mode-indent-xr-files/indent_xr_statement_body.m
     ((n-p-gp nil ,(rx bos "\n" eos)
              ,(rx bos (or "class_definition"
                           "properties"
                           "enumeration"
                           "methods"
                           "events"
                           "function_definition"
                           "arguments_statement"
                           "spmd_statement"
                           "try_statement"
                           "catch_clause"
                           "while_statement"
                           "for_statement"
                           "if_statement" "else_clause" "elseif_clause")
                   eos))
      grand-parent ,matlab-ts-mode--indent-level)

     ;; I-Rule: disp(myMatrix(1:  ...
     ;; <TAB>                 end));
     ((parent-is ,(rx bos "range" eos)) parent 0)

     ;; I-Rule: try<RET>    |   catch<RET>
     ((parent-is ,(rx bos (or "try_statement" "catch_clause") eos))
      parent ,matlab-ts-mode--indent-level)

     ;; I-Rule: function a
     ;;             x = 1;
     ;; <TAB>       y = 2;
     ((parent-is ,(rx bos "block" eos)) parent 0)

     ;; I-Rule:  a = ...
     ;; <TAB>        1;
     ((parent-is ,(rx bos "assignment" eos)) parent ,matlab-ts-mode--indent-level)

     ;; I-Rule:  a = 2 * ...
     ;; <TAB>        1;
     ((parent-is ,(rx bos "binary_operator" eos)) parent 0)

     ;; I-Rule:  a = ( ...       |     a = [  ...     |     a = {    ...
     ;;               1 ...      |          1 ...     |            1 ...
     ;; <TAB>        );          |         ];         |         };
     ((node-is ,(rx bos (or ")" "]" "}") eos)) parent 0)

     ;; I-Rule:  a = ( ...
     ;; <TAB>         1 ...
     ((parent-is ,(rx bos "parenthesis" eos)) parent 1)

     ;; I-Rule:  a = [   ...    |    a = { ...
     ;; <TAB>          2 ...    |          2 ...
     ;; See: tests/test-matlab-ts-mode-indent-files/indent_matrix.m
     ((parent-is ,(rx bos (or "cell" "matrix") eos)) parent ,#'matlab-ts-mode--row-indent-level)

     ;; I-Rule:   cell1 = { ...
     ;; See: ./test-matlab-ts-mode-indent-xr-files/indent_xr_cell1.m
     ((n-p-gp nil ,(rx bos "line_continuation" eos) ,(rx bos (or "cell" "matrix") eos))
      grand-parent ,#'matlab-ts-mode--row-indent-level)

     ;; I-Rule:  function [   ...              |    function name (   ...
     ;; <TAB>              a, ... % comment    |                   a, ... % comment
     ((parent-is ,(rx bos (or "multioutput_variable" "function_arguments") eos)) parent 1)

     ;; I-Rule:  a = [    2 ...       |   function a ...
     ;; <TAB>             1 ...       |            = fcn
     ((parent-is ,(rx bos (or "row" "function_output") eos)) parent 0)

     ;; I-Rule:  a = ...
     ;; <TAB>        1;
     ((n-p-gp nil nil ,(rx bos "assignment" eos)) grand-parent ,matlab-ts-mode--indent-level)

     ;; I-Rule:  a = my_function(1, ...
     ;; <TAB>                    2, ...
     ((parent-is ,(rx bos "arguments" eos)) parent 0)

     ;; I-Rule:  my_function( ...
     ;; <TAB>        1, ...
     ((node-is ,(rx bos "arguments" eos)) parent ,matlab-ts-mode--indent-level)

     ;; I-Rule: function indent_tab_between_fcns   |   function indent_tab_in_fcn
     ;;         end                                |      disp('here')
     ;; <TAB>                                      |
     ;;         function b                         |   end
     ;;         end                                |
     ((lambda (node parent bol)
        (and (not parent)
             (string= (treesit-node-type parent) "\n")))
      grand-parent 0)

     ;; I-Rule: comments in classdef's
     ((parent-is ,(rx bos "class_definition" eos)) parent ,matlab-ts-mode--indent-level)

     ;; I-Rule: comment after property:
     ;;         arguments
     ;;             a
     ;;    TAB>     % comment
     ;;         end
     ;; See: tests/test-matlab-ts-mode-indent-files/indent_comment_after_prop.m
     ((node-is "comment") parent 0)

     ;; I-Rule: error-switch-matcher
     ;;         switch fcn1(a)
     ;;           ^                       <== TAB to here
     (,#'matlab-ts-mode--i-error-switch-matcher
      ,#'matlab-ts-mode--i-error-switch-anchor
      ,#'matlab-ts-mode--i-error-switch-offset)

     ;; I-Rule: line continuation for valid statements
     ;;         arguments
     ;;             a (1,1) ... comment
     ;;    TAB>        double
     ;;         endo
     ;; See: tests/test-matlab-ts-mode-indent-files/indent_line_continuation.m
     ;; See: tests/test-matlab-ts-mode-indent-files/indent_line_continuation_row.m
     (,#'matlab-ts-mode--i-cont-matcher parent ,#'matlab-ts-mode--i-cont-offset)

     ;; I-Rule: line continuations for incomplete continuations
     ;;            myVariable = 1 + myfcn(a, ...
     ;;                                   ^              <== TAB or RET on prior line goes here
     ;;
     ;;            myVariable = 1 + myfcn(a, b) + ...
     ;;                ^                                 <== TAB or RET on prior line goes here
     ;; See: tests/test-matlab-ts-mode-indent-xr-files/indent_xr_i_cont_incomplete*.m
     (,#'matlab-ts-mode--i-cont-incomplete-matcher
      ,#'matlab-ts-mode--i-cont-incomplete-anchor
      ,#'matlab-ts-mode--i-cont-incomplete-offset)

     ;; I-Rule: Assert if no rule matched and asserts are enabled.
     ,matlab-ts-mode--indent-assert-rule
     ))
  "Tree-sitter indent rules for `matlab-ts-mode'.")

;;; Thing settings for movement, etc.

;; TODO should we use following for M-a, M-e?
;; This needs tune up, but could workout better than using matlab-ts-mode--thing-settings
;;
;; (defvar matlab-ts-mode--statements-ht
;;   #s(hash-table
;;      test equal
;;      data ("arguments_statement" t
;;            "assignment" t
;;            "lambda" t
;;            "class_definition" t
;;            "enumeration" t
;;            "events" t
;;            "for_statement" t
;;            "function_definition" t
;;            "if_statement" t
;;            "methods" t
;;            "property" t
;;            "properties" t
;;            "spmd_statement" t
;;            "switch_statement" t
;;            "try_statement" t
;;            "while_statement" t))
;;   "MATLAB command statements.")
;;
;; (cl-defun matlab-ts-mode-beginning-of-statement (&optional goto-end)
;;   "Move to the beginning of a command statement.
;; If optional GOTO-END is \\='end, move to end of the current statement.
;;
;; We define a command statement to be a complete syntatic unit that has
;; a start and end.  For example, if point is in an assigment statement
;;        var = ...
;;          1;
;; move point to the \"v\" when GOTO-END is nil, otherwise move to the
;; \";\".  If point is in an if statement, move to the start or end of
;; that.  Likewise for other command statements.
;;
;; The point is moved to the start or end of the innermost statement that
;; the point is on.  No movement is performed if point is not in a
;; statement.  This can occur when there are syntax errors or the buffer
;; has no content.
;;
;; Returns nil if not in a statement, otherwise the `point' which
;; will be a new point if the starting point was not at the start
;; or end of the command statement."
;;   (interactive)
;;
;;   (cl-assert (or (not goto-end) (eq goto-end 'end)))
;;
;;   (let ((node (treesit-node-at (point))))
;;
;;     (when (and (> (point) 1)
;;                (equal (treesit-node-type node) "\n")
;;                (re-search-backward "[^ \t\n\r]" nil t))
;;       (setq node (treesit-node-at (point))))
;;
;;     (while (and node
;;                 (let ((type (treesit-node-type node)))
;;                   (when (equal type "ERROR")
;;                     ;; No movement if we have a syntax error
;;                     (message "Not in statement due to syntax error.")
;;                     (cl-return nil))
;;                   (not (gethash type matlab-ts-mode--statements-ht))))
;;       (setq node (treesit-node-parent node)))
;;
;;     (when (not node)
;;       (message "Not in a statement.")
;;       (cl-return))
;;
;;     (when node
;;       (goto-char (if (eq goto-end 'end)
;;                      (treesit-node-end node)
;;                    (treesit-node-start node))))))
;;
;; (defun matlab-ts-mode-end-of-statement ()
;;   "Move to the end of a command statement.
;; This is the opposite of `matlab-ts-mode-beginning-of-statement'."
;;   (interactive)
;;   (matlab-ts-mode-beginning-of-statement 'end))

(defvar matlab-ts-mode--thing-settings
  `((matlab
     (defun ,(rx bos "function_definition" eos))
     (sexp ,(rx bos (or "function_definition"
                        "arguments_statement"

                        ;; "property" of "arguments_statement" and "properties"
                        ;;
                        ;; TODO: the property node includes a newline, which means C-M-f jumps to
                        ;; an odd location.  Should the matlab tree sitter grammar be updated or
                        ;; should we special case this?
                        ;;     function method1(in1, in2)
                        ;;         arguments
                        ;;             in1 (1,1) double
                        ;;             ^                         % <- point here, C-M-f
                        ;;             in2 (1,2) double
                        ;;     ^                                 % -> moves point here.
                        ;;         end
                        ;;
                        ;; Here's the nodes:
                        ;;     (property name: (identifier)
                        ;;      (dimensions ( (number) , (number) ))
                        ;;      (identifier) \n)
                        ;;     (property name: (identifier)
                        ;;      (dimensions ( (number) , (number) ))
                        ;;      (identifier) \n)
                        "property"

                        "function_arguments"
                        "function_call"

                        "assignment"
                        "break_statement"
                        "cell"
                        "continue_statement"
                        "command"
                        "for_statement"
                        "global_operator"
                        "if_statement"
                        "matrix"
                        "persistent_operator"
                        "return_statement"
                        "switch_statement"
                        "case"
                        "otherwise"
                        "catch"
                        "try_statement"
                        "while_statement"

                        "class_definition"
                        "classdef"
                        "properties"
                        "methods"
                        "events"
                        "enumeration"
                        "enum"

                        "identifier")
                eos))

     (sentence ,(rx bos (or "function_definition"
                            "assignment"
                            "arguments_statement"
                            "for_statement"
                            "if_statement"
                            "switch_statement"
                            "try_statement"
                            "while_statement"
                            "class_definition"
                            "properties"
                            "methods"
                            "events"
                            "enumeration")))
     ;; TODO is "line_continuation" correct here - add test
     (text ,(rx bos (or "comment" "string" "line_continuation") eos))

     ))
  "Tree-sitter things for movement.")

(defun matlab-ts-mode--forward-sexp (&optional arg)
  "Use `treesit-forward-sexp' when not in comments.
ARG is described in the docstring of `forward-sexp-function'.
When in comments do the normal parenthesis s-expression movement
by calling `forward-sexp-default-function'."
  (interactive "^p")
  (let* ((pt-and-node (matlab-ts-mode--real-node-at-point))
         (node (cdr pt-and-node)))
    (if (equal (treesit-node-type node) "comment")
        (forward-sexp-default-function arg)
      (treesit-forward-sexp arg))))

;;; Change Log

(defun matlab-ts-mode--defun-name (node)
  "Return the defun name of NODE for Change Log entries."
    (when (string-match-p
           (rx bos (or "function_definition" "class_definition") eos)
           (treesit-node-type node))
      (treesit-node-text (treesit-node-child-by-field-name node "name"))))

;;; imenu

(defun matlab-ts-mode--imenu-create-index ()
  "Return index for imenu.
This will return alist of functions and classes in the current buffer:
   \\='((\"function1\" . start-point1)
     (\"function2\" . start-point2)"
  (let ((root (treesit-buffer-root-node))
        (index '()))
    (treesit-search-subtree
     root
     (lambda (node)
       (let ((type (treesit-node-type node)))
         (pcase type

           ("class_definition"
            (let* ((name (concat
                          "-classdef:"
                          (treesit-node-text (treesit-node-child-by-field-name node "name"))))
                   (start-pt (treesit-node-start node)))
              (push `(,name . ,start-pt) index)))

           ("function_definition"
            (let* ((name-node (treesit-node-child-by-field-name node "name"))
                   (prev-type (treesit-node-type (treesit-node-prev-sibling name-node)))
                   (name (treesit-node-text name-node))
                   (start-pt (treesit-node-start node))
                   (parent (treesit-node-parent node)))

              ;; classdef get.propName or set.propName
              (when (and prev-type
                         (string-match-p (rx bos (or "get." "set.") eos) prev-type))
                (setq name (concat prev-type name)))

              ;; If nested function prefix with that by looking to parent
              (while parent
                (when (string= "function_definition" (treesit-node-type parent))
                  (let ((parent-name
                         (treesit-node-text (treesit-node-child-by-field-name parent "name"))))
                    (setq name (concat parent-name "." name))))
                (setq parent (treesit-node-parent parent)))
              (push `(,name . ,start-pt) index))))
         nil)))
    index))

;;; Outline minor mode, M-x outline-minor-mode

(defun matlab-ts-mode--outline-predicate (node)
  "Outline headings for `outline-minor-mode' with MATLAB.
Returns t if tree-sitter NODE defines an outline heading."
  (let ((node-type (treesit-node-type node)))
    (or (string-match-p (rx bos (or "function_definition" "class_definition") eos) node-type)
        (and (string= "comment" node-type)
             (save-excursion
               (goto-char (treesit-node-start node))
               (beginning-of-line)
               (looking-at matlab-ts-mode--comment-heading-re))))))

;;; Save hooks

(defun matlab-ts-mode--highlight-ask (begin end prompt)
  "Highlight from BEGIN to END while asking PROMPT as a yes-no question."
  (let ((mo (make-overlay begin end (current-buffer)))
        (show-paren-mode nil) ;; this will highlight things we often ask about.  disable.
        ans)
    (condition-case nil
        (progn
          (overlay-put mo 'face 'matlab-region-face)
          (setq ans (y-or-n-p prompt))
          (delete-overlay mo))
      (quit (delete-overlay mo)
            (error "Quit")))
    ans))

(defun matlab-ts-mode-on-save-fix-name (&optional no-prompt)
  "If file name and function/classdef name are different, offer to fix.
If optional NO-PROMPT is t, fix the name if needed without prompting."
  (interactive)
  (when (or no-prompt (not noninteractive)) ;; can only prompt if in interactive mode
    (let* ((root (treesit-buffer-root-node))
           (children (treesit-node-children root)))
      (cl-loop for child in children do
               (let ((child-type (treesit-node-type child)))
                 (cond
                  ;; Case: function_definition or class_definition
                  ((string-match-p (rx bos (or "function_definition" "class_definition") eos)
                                   child-type)
                   (let* ((def-name-node (treesit-node-child-by-field-name child "name"))
                          (def-name (treesit-node-text def-name-node))
                          (file-name (file-name-nondirectory (or (buffer-file-name) (buffer-name))))
                          (base-name-no-ext (replace-regexp-in-string "\\.[^.]+\\'" "" file-name)))
                     ;; When base-name-no-ext is a valid name, MATLAB will use that.
                     ;; Invalid file names result in an error in MATLAB, so don't try to fix
                     ;; the function/classdef name in that case.
                     (when (and (string-match-p "\\`[a-zA-Z][a-zA-Z0-9_]*\\'" base-name-no-ext)
                                (not (string= def-name base-name-no-ext)))
                       (let ((start-pt (treesit-node-start def-name-node))
                             (end-pt (treesit-node-end def-name-node)))
                         (when (or no-prompt
                                   (matlab-ts-mode--highlight-ask
                                    start-pt end-pt
                                    (concat (if (string= child-type "function_definition")
                                                "Function"
                                              "Classdef")
                                            " name and file names are different. Fix?")))
                           (save-excursion
                             (goto-char start-pt)
                             (delete-region start-pt end-pt)
                             (insert base-name-no-ext))))))
                   (cl-return))

                  ;; Case: anthing except a comment
                  ((not (string= "comment" child-type))
                   (cl-return))))))))

(defun matlab-ts-mode--write-file-callback ()
  "Called from `write-contents-functions'.
When `matlab-verify-on-save-flag' is true, run `matlab-mode-verify-fix-file'.
Enable/disable `matlab-sections-minor-mode' based on file content."
  (mapc (lambda (fix-function)
          (funcall fix-function))
        matlab-ts-mode-on-save-fixes)
  ;; `write-contents-functions' expects this callback to return nil to continue with other hooks and
  ;; the final save. See `run-hook-with-args-until-success'.
  nil)

;;; Electric Pair Mode, M-x electric-pair-mode

(declare-function electric-pair-default-inhibit "elec-pair")

(defun matlab-ts-mode--electric-pair-inhibit-predicate (char)
  "Return non-nil if `electric-pair-mode' should not pair this CHAR.
Do not pair the transpose operator, (\\='), but pair it when used as a
single quote string."

  ;; (point) is just after CHAR. For example, if we type a single quote:
  ;;   x = '
  ;;        ^--(point)

  (cond
   ;; Case: Single quote
   ;;  str = '      : start of single quote string => nil
   ;;  mat'         : transpose operator => t
   ;;  str = " ' "  : add single quote in string => t
   ((eq char ?')
    (let* ((node-back1 (treesit-node-at (- (point) 1)))
           (type-back1 (treesit-node-type node-back1)))
      (cond
       ;; Case: in comment, return t if it looks like a transpose, e.g. A' or similar.
       ((string-match-p (rx bos (or "comment" "line_continuation") eos) type-back1)
        (save-excursion
          (forward-char -1)
          (looking-at "\\w\\|\\s_\\|\\.")))

       ;; Case: string delimiter
       ;;    double up if starting a new string => return nil
       ((string= "'" type-back1 )
        (let ((parent-back1-type (treesit-node-type (treesit-node-parent node-back1))))
          ;; When we type
          ;;   s = '
          ;; we'll get a syntax error from the parse-tree.  Older versions of the
          ;; matlab tree-sitter would return string so we keep that condition.
          (not (or (string= "ERROR" parent-back1-type)
                   (string= "string" parent-back1-type)))))

       ;; Case: inside a single quote string
       ;;    s = 'foobar'
       ;;            ^ insert here
       ;;    s = 'foo''bar'
       ((and (string= "string_content" type-back1)
             (string= "'" (treesit-node-type (treesit-node-prev-sibling node-back1))))
        nil)

       ;; Case: not a string delimiter, return t
       ;;   transpose:                  A'
       ;;   inside double quote string: "foo'bar"
       (t
        t))))

   ;; Case: Not a single quote, defer to the standard electric pair handling
   (t
    (funcall #'electric-pair-default-inhibit char))))

;;; show-paren-mode

(declare-function show-paren--default "paren")

(defun matlab-ts-mode--show-paren-or-block ()
  "Function to assign to `show-paren-data-function'.
Highlight MATLAB pairs in addition to standard items paired by
`show-paren-mode'.  Returns a list: \\='(HERE-BEGIN HERE-END THERE-BEGIN
THERE-END MISMATCH) or nil."
  (let* (here-begin
         here-end
         there-begin
         there-end
         mismatch
         (pt-and-node (matlab-ts-mode--real-node-at-point))
         (pt (car pt-and-node))
         (node (cdr pt-and-node)))

    ;; If point is in whitespace, (treesit-node-at (point)) returns the nearest node. For
    ;; paired matching we want the point on either a start or end paired item.
    (let ((node-start (treesit-node-start node))
          (node-end (treesit-node-end node)))
      (when (and (>= pt node-start)
                 (<= pt node-end))
        (let* ((start-to-parent '(("classdef"    . "class_definition")
                                  ("methods"     . "methods")
                                  ("properties"  . "properties")
                                  ("enumeration" . "enumeration")
                                  ("events"      . "events")
                                  ("function"    . "function_definition")
                                  ("arguments"   . "arguments_statement")
                                  ("if"          . "if_statement")
                                  ("switch"      . "switch_statement")
                                  ("for"         . "for_statement")
                                  ("parfor"      . "for_statement")
                                  ("while"       . "while_statement")
                                  ("try"         . "try_statement")
                                  ("spmd"        . "spmd_statement")
                                  ))
               (start-node-types (mapcar (lambda (pair) (car pair)) start-to-parent))
               (start-re (rx-to-string `(seq bos (or ,@start-node-types) eos) t))
               (node-type (treesit-node-type node))
               (parent-node (treesit-node-parent node))
               (parent-type (treesit-node-type parent-node)))

          (cond

           ;; Case: on a start node, e.g. "function"
           ((string-match-p start-re node-type)
            (let* ((expected-parent-type (cdr (assoc node-type start-to-parent)))
                   (expected-end-type "end") ;; all blocks terminate with an "end" statement
                   (end-node (car (last (treesit-node-children parent-node)))))
              (setq here-begin (treesit-node-start node)
                    here-end (treesit-node-end node))
              (if (and (equal (treesit-node-type parent-node) expected-parent-type)
                       (equal (treesit-node-type end-node) expected-end-type))
                  (setq there-begin (treesit-node-start end-node)
                        there-end (treesit-node-end end-node))
                ;; A missing end keyword will result in parent being "ERROR".
                ;; See: tests/test-matlab-ts-mode-show-paren-files/show_paren_classdef_missing_end.m
                (setq mismatch t))))

           ;; Case: on a "end" node or an inner block that should be matcheed with parent
           ((and (string-match-p (rx bos (or "end" "elseif" "else" "case" "otherwise" "catch") eos)
                                 node-type)
                 ;; and we have a matching parent node
                 (progn
                   (when (not (equal node-type "end"))
                     (setq parent-node (treesit-node-parent parent-node))
                     (setq parent-type (treesit-node-type parent-node)))
                   (let ((parents (mapcar (lambda (pair)
                                            (cdr pair))
                                          start-to-parent)))
                     (member parent-type parents))))
            (let ((parent-to-start (mapcar (lambda (pair)
                                             (cons (cdr pair) (car pair)))
                                           start-to-parent)))

              (setq here-begin (treesit-node-start node))
              (setq here-end (treesit-node-end node))
              (setq there-begin (treesit-node-start parent-node))
              (setq there-end (+ there-begin (length (cdr (assoc parent-type parent-to-start)))))))

           ;; Case: on a single or double quote for a string.
           ((and (or (equal "'" node-type)
                     (equal "\"" node-type))
                 (equal "string" parent-type))
            (let (q-start-node
                  q-end-node)
              (if (= (treesit-node-start parent-node) (treesit-node-start node))
                  ;; looking at start quote
                  (setq q-start-node node
                        q-end-node parent-node)
                ;; else looking at end quote
                (setq q-start-node parent-node
                      q-end-node node))

              (setq here-begin (treesit-node-start q-start-node))
              (setq here-end (1+ here-begin))

              (let* ((candidate-there-end (treesit-node-end q-end-node))
                     (candidate-there-begin (1- candidate-there-end)))
                (cond
                 ;; Case: Have starting quote of a string, but no content or closing quote.
                 ((= here-begin candidate-there-begin)
                  (setq mismatch t))
                 ;; Case: Have starting quote, have string content, but no closing quote
                 ((not (equal (char-after here-begin) (char-after candidate-there-begin)))
                  (setq mismatch t))
                 (t
                  (setq there-begin candidate-there-begin)
                  (setq there-end candidate-there-end))))))

           ))))

    (if (or here-begin here-end)
        (list here-begin here-end there-begin there-end mismatch)
     (funcall #'show-paren--default))))

;;; post-command-hook

(defun matlab-ts-mode--post-command-newline ()
  "Ensure buffer always has a newline.
The matlab tree-sitter requires a newline, see
https://github.com/acristoffers/tree-sitter-matlab/issues/34"
  (when (and (eq this-command 'self-insert-command)
             (eq major-mode 'matlab-ts-mode))
    (save-excursion
      (goto-char (point-max))
      (when (not (= (char-before) ?\n))
        (insert "\n")))))

;;; Key bindings

;; TODO - update matlab-shell to use matlab-mode (matlab.el) or matlab-ts-mode.el functions
;;        then enable following. Double check against matlab.el key bindings to see if we
;;        want others.
;;
;; (defvar matlab-ts-mode--help-map
;;   (let ((km (make-sparse-keymap)))
;;     (define-key km "r" #'matlab-shell-run-command)
;;     (define-key km "f" #'matlab-shell-describe-command)
;;     (define-key km "a" #'matlab-shell-apropos)
;;     (define-key km "v" #'matlab-shell-describe-variable)
;;     km)
;;   "The help key map for `matlab-ts-mode' and `matlab-shell-mode'.")
;;
;; (defvar matlab-ts-mode--map
;;   (let ((km (make-sparse-keymap)))
;;
;;     ;; TODO - keep? Insert, Fill stuff
;;     ;; (define-key km [(control c) (control c)] 'matlab-insert-map-fcn)
;;     ;; (define-key km [(control c) (control j)] 'matlab-justify-line)
;;
;;     ;; Connecting to MATLAB Shell
;;     (define-key km [(control c) (control s)] #'matlab-shell-save-and-go)
;;     (define-key km [(control c) (control r)] #'matlab-shell-run-region)
;;     (define-key km [(meta control return)] #'matlab-shell-run-code-section)
;;     (define-key km [(control return)] #'matlab-shell-run-region-or-line)
;;     (define-key km [(control c) (control t)] #'matlab-show-line-info)
;;     (define-key km [(control c) ?. ] #'matlab-shell-locate-fcn)
;;     (define-key km [(control h) (control m)] matlab-ts-mode--help-map)
;;     (define-key km [(meta s)] #'matlab-show-matlab-shell-buffer)
;;     (define-key km [(control meta mouse-2)] #'matlab-find-file-click)
;;
;;     ;; Debugger interconnect
;;     (substitute-key-definition 'read-only-mode 'matlab-toggle-read-only km global-map)
;;
;;     km)
;;   "The keymap used in `matlab-ts-mode'.")

;;; matlab-ts-mode

;;;###autoload
(define-derived-mode matlab-ts-mode prog-mode "MATLAB:ts"
  "Major mode for editing MATLAB files with tree-sitter.

This mode is independent from the classic matlab-mode.el, `matlab-mode',
so configuration variables of that mode, like do not affect this mode.

If you have the MATLAB tree-sitter grammar installed,
  (treesit-ready-p \\='matlab)
is t, add the following to an Init File (e.g. `user-init-file' or
`site-run-file') to enter the MATLAB tree-sitter mode by default:

  (add-to-list \\='major-mode-remap-alist \\='(matlab-mode . matlab-ts-mode))"

  (matlab-ts-mode--check-file-encoding)

  (when (treesit-ready-p 'matlab)
    (treesit-parser-create 'matlab)

    ;; Syntax table - think of this as a "language character descriptor". It tells us what
    ;; characters belong to word like things giving us movement commands e.g. C-M-f, matching
    ;; parens, `show-paren-mode', etc.
    ;; See: ./tests/test-matlab-ts-mode-syntax-table.el
    (set-syntax-table matlab-ts-mode--syntax-table)
    (setq-local syntax-propertize-function #'matlab-ts-mode--syntax-propertize)

    ;; Comments.
    ;; See: tests/test-matlab-ts-mode-comments.el
    (setq-local comment-start      "% ")
    (setq-local comment-end        "")
    (setq-local comment-start-skip "%\\s-+")

    ;; Setup `forward-page' and `backward-page' to use ^L or "%% heading" comments
    ;; See: ./tests/test-matlab-ts-mode-page.el
    (setq-local page-delimiter "^\\(?:\f\\|%%\\(?:\\s-\\|\n\\)\\)")

    ;; Font-lock.
    ;; See: ./tests/test-matlab-ts-mode-font-lock.el
    (setq-local treesit-font-lock-level matlab-ts-mode-font-lock-level)
    (setq-local treesit-font-lock-settings matlab-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '((comment comment-special comment-marker definition fcn-name-value)
                  (keyword operator string type command-name command-arg)
                  (variable builtins namespace-builtins number bracket delimiter)
                  (syntax-error)))

    ;; Indent.
    ;; See: ./tests/test-matlab-ts-mode-indent.el
    (matlab-ts-mode--set-function-indent-level) ;; Set function indent level on file load
    (setq-local indent-tabs-mode nil) ;; For consistency between Unix and Windows we don't use TABs.
    (setq-local treesit-simple-indent-rules
                (if treesit--indent-verbose ;; add debugging print as first rule?
                    (list (append `,(list (caar matlab-ts-mode--indent-rules))
                                  (list matlab-ts--indent-debug-rule)
                                  (cdar matlab-ts-mode--indent-rules)))
                  matlab-ts-mode--indent-rules))

    ;; Thing settings for movement, etc.
    ;; See: tests/test-matlab-ts-mode-thing-settings.el
    (setq-local treesit-thing-settings matlab-ts-mode--thing-settings)

    ;; Change Logs.
    ;; See: tests/test-matlab-ts-mode-treesit-defun-name.el
    (setq-local treesit-defun-name-function #'matlab-ts-mode--defun-name)

    ;; M-x imenu
    ;; See: ./tests/test-matlab-ts-mode-imenu.el
    ;;
    ;; TODO - lsp-mode and imenu
    ;; I think we need (setq-local lsp-enable-imenu nil) when lsp-mode is used.  Can we find a
    ;; automatic way to do this? See:
    ;; https://www.reddit.com/r/emacs/comments/1c216kr/experimenting_with_tree_sitter_and_imenulist/
    ;; At minimum we should doc this.
    (setq-local imenu-create-index-function #'matlab-ts-mode--imenu-create-index)

    ;; M-x outline-minor-mode
    ;; See: ./tests/test-matlab-ts-mode-outline.el
    (setq-local treesit-outline-predicate #'matlab-ts-mode--outline-predicate)

    ;; Save hook.
    ;; See: ./tests/test-matlab-ts-mode-on-save-fixes.el
    (add-hook 'write-contents-functions #'matlab-ts-mode--write-file-callback -99 t)

    ;; Electric pair mode.
    ;; See tests/test-matlab-ts-mode-electric-pair.el
    (setq-local electric-pair-inhibit-predicate #'matlab-ts-mode--electric-pair-inhibit-predicate)

    ;; show-paren-mode. Highlight parens OR function/end, if/end, etc. type blocks.
    ;; See: tests/test-matlab-ts-mode-show-paren.el
    (setq-local show-paren-data-function #'matlab-ts-mode--show-paren-or-block)

    ;; Final newline.  The matlab tree-sitter requires a final newline, see
    ;;    https://github.com/acristoffers/tree-sitter-matlab/issues/34
    ;; Setting require-final-newline to 'visit-save doesn't guarantee we have a newline when typing
    ;; (inserting text), so we also setup a post-command-hook to insert a newline if needed.
    (setq-local require-final-newline 'visit-save)
    (add-hook 'post-command-hook #'matlab-ts-mode--post-command-newline -99 t)

    ;; TODO the MATLAB menu items from matlab.el, e.g. debugging, etc.
    ;;      - will need to update matlab-shell.el to either use matlab.el or matlab-ts-mode.el
    ;;
    ;; TODO update matlab-ts-mode--builtins.el. I generated using R2025a installation, though I
    ;;      think it was missing a few toolboxes.
    ;;
    ;; TODO double check t-utils.el help, extract the help and put in treesit how to
    ;;
    ;; TODO indent
    ;;      function outResult = foo
    ;;                  outResult = longFunction(...    <== TAB should shift, w/o "..." it shifts
    ;;      end
    ;;      Requires ERROR node be located under the function_output.
    ;;      See https://github.com/acristoffers/tree-sitter-matlab/issues/47
    ;;
    ;; TODO indent
    ;;      filesToCheck = ...
    ;;          [
    ;;            ^                <== RET on previous line or TAB should be here
    ;;      See https://github.com/acristoffers/tree-sitter-matlab/issues/47
    ;;
    ;; TODO font-lock
    ;;      classdef fooenum
    ;;          enumeration
    ;;
    ;;              red  % bad font when there's a blank line before this member
    ;;          end
    ;;      end
    ;;
    ;; TODO Mismatched parentheses
    ;;      Start with:
    ;;        Line1:  mat = [ [1, 2]; [3, 4];
    ;;        Line2:
    ;;      then type a ']' on the 2nd line, in echo area, see: Mismatched parentheses
    ;;
    ;; TODO create defcustom matlab-ts-mode-electric-ends that inserts end statements
    ;;      when a function, switch, while, for, etc. is entered. This should handle continuations.

    (treesit-major-mode-setup)

    ;; Correct forward-sexp setup created by `treesit-major-mode' so that in comments we do normal
    ;; s-expression matching using parenthesis. This fix is need for our tests work. We need
    ;; to evaluate (t-utils-NAME ....) expressions from within comments using C-x C-e and this
    ;; leverages forward-sexp to match up the parentheses.
    (setq-local forward-sexp-function #'matlab-ts-mode--forward-sexp)
    ))

(provide 'matlab-ts-mode)
;;; matlab-ts-mode.el ends here
