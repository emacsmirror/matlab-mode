;;; matlab-ts-mode.el --- MATLAB(R) Tree-Sitter Mode -*- lexical-binding: t -*-

;; Copyright 2025 Free Software Foundation, Inc.
;;
;; URL: https://github.com/mathworks/Emacs-MATLAB-Mode
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Author: John Ciolfi <john.ciolfi.32@gmail.com>
;; Created: Jun-24-2025
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
  "Face for \"%% code section\" headings when NOT in matlab-sections-minor-mode.")

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

(defcustom matlab-ts-mode-font-lock-level 4
  "*Level of font lock, 1 for minimal syntax highlighting and 4 for maximum."
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
                                         (rx bol "line_continuation" eol)
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
    "switch"
    "try"
    "while")
  "The matlab-ts-mode font-lock keywords.")

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
      (while (string-match-p (rx bol "line_continuation" eol)
			     (treesit-node-type prev-node))
        (setq prev-node (treesit-node-prev-sibling prev-node)))
      (let ((prev-type (treesit-node-type prev-node)))
        ;; The true (t) cases. Note line continuation ellipsis are allowed.
        ;;    function foo          function foo(a)
        ;;    % doc comment         % doc comment
        ;;    end                   end
        (when (or (string-match-p (rx bol (or
                                           "function_arguments" ;; function input args?
                                           "superclasses")      ;; subclass
                                      eol)
                                  prev-type)
	          (and (string= prev-type "identifier")           ;; id could be a fcn or class id
	               (string-match-p (rx bol
                                           (or "function"         ;; fcn wihtout in and out args
                                               "function_output"  ;; fcn w/out args and no in args
                                               "classdef")        ;; base class
                                           eol)
                                       (treesit-node-type (treesit-node-prev-sibling prev-node)))))
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
             (string-match-p (rx bol (or "function_definition" "class_definition") eol)
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
fontified which could be smaller or larger START and END specify the
region to be fontified."
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

(defvar matlab-ts-mode--font-lock-settings
  (treesit-font-lock-rules

   ;; F-Rule: Comments and line continuation: ... optional text
   :language 'matlab
   :feature 'comment
   '((comment) @font-lock-comment-face
     (line_continuation) @font-lock-comment-face)

   ;; F-Rule: special comments that override normal comment font
   :language 'matlab
   :feature 'comment
   :override t
   '(((comment) @matlab-ts-mode-pragma-face
      (:match "^%#.+$" @matlab-ts-mode-pragma-face)) ;; %#pragma's
     ((comment) @matlab-ts-mode--comment-heading-capture) ;; %% comment heading

     (function_definition (comment) @matlab-ts-mode--doc-comment-capture) ;; doc help comments
     (class_definition (comment) @matlab-ts-mode--doc-comment-capture)) ;; doc help comments

   ;; F-Rule: to do, fix me, triple-x marker comment keywords
   :language 'matlab
   :feature 'comment
   :override t
   '(((comment) @matlab-ts-mode--comment-to-do-capture))

   ;; F-Rule: keywords: if, else, etc.
   :language 'matlab
   :feature 'keyword
   `([,@matlab-ts-mode--keywords] @font-lock-keyword-face)

   ;; F-Rule: function/classdef and items definiting them, e.g. the function arguments
   :language 'matlab
   :feature 'definition
   '((function_definition name: (identifier) @font-lock-function-name-face)
     (class_definition name: (identifier) @font-lock-function-name-face)
     (superclasses (property_name (identifier)) @font-lock-function-name-face)
     ;; Function inputs: functionName(in1, in2, in3)
     (function_arguments arguments:
                         (identifier)      @font-lock-variable-name-face
                         ("," (identifier) @font-lock-variable-name-face) :*)
     ;; Function single output argument: function out = functionName(in1, in2)
     (function_output (identifier) @font-lock-variable-name-face)
     ;; Function multiple output arguments: function [out1, out2] = functionName(in1, in2)
     (function_output (multioutput_variable (identifier) @font-lock-variable-name-face))
     ;; Fields of: arguments ... end , properties ... end
     (property (validation_functions (identifier) @font-lock-builtin-face))
     (property name: (identifier) @font-lock-property-name-face
               (identifier) @font-lock-type-face :?)
     (property name: (property_name (identifier) @font-lock-property-name-face)
               (identifier) @font-lock-type-face :?)
     ;; (property name: (property_name (identifier) @font-lock-property-name-face))
     ;; Enumeration's
     (enum (identifier) @font-lock-property-name-face)
     ;; events block in classdef
     (events (identifier) @font-lock-property-name-face)
     ;; attributes of properties, methods
     (attribute (identifier) @font-lock-type-face "=" (identifier) @font-lock-builtin-face)
     (attribute (identifier) @font-lock-type-face))

   ;; F-Rule: strings "double quote" and 'single quote'
   :language 'matlab
   :feature 'string
   '((string_content) @font-lock-string-face
     ((string_content) ["\"" "'"]) @matlab-ts-mode-string-delimiter-face
     (string ["\"" "'"] @matlab-ts-mode-string-delimiter-face))

   ;; F-Rule: transpose uses "'" after an identifier, e.g. for matrix A we tranpose it via: A'
   ;; since "'" is also used as a string, we use a different face for transpose and put it under
   ;; the string category.
   :language 'matlab
   :feature 'string
   '((postfix_operator "'" @font-lock-function-name-face))

   ;; F-Rule: Types, e.g. int32()
   :language 'matlab
   :feature 'type
   `((function_call name: (identifier)
                    @font-lock-type-face
                    (:match ,(rx-to-string
                              `(seq bol
                                    (or ,@matlab-ts-mode--type-functions)
                                    eol))
                              @font-lock-type-face)))

   ;; F-Rule: Constant literal numbers, e.g. 1234
   :language 'matlab
   :feature 'number
   '((number) @font-lock-constant-face)

   ;; F-Rule: Brackets
   :language 'matlab
   :feature 'bracket
   '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

   ;; F-Rule: Delimiters, e.g. semicolon
   :language 'matlab
   :feature 'delimiter
   '((["," "." ";" ":" "@" "?"]) @font-lock-delimiter-face)

   ;; F-Rule: Syntax errors
   :language 'matlab
   :feature 'syntax-error
   :override t
   '((ERROR) @font-lock-warning-face)

   )
  "The matlab-ts-mode font-lock settings.")


;;; Indent

;; We discourage customizing the indentation rules. Having one-style of consistent indentation makes
;; reading others' code easier.
(defvar matlab-ts-mode--indent-level 4
  "Indentation level.")
(defvar matlab-ts-mode--switch-indent-level (/ matlab-ts-mode--indent-level 2)
  "Indentation level for switch-case statements.")
(defvar matlab-ts-mode--array-indent-level 2
  "Indentation level for elements in an array.")

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

  (let ((root (if (and parent (string= (treesit-node-type parent) "function_definition"))
                  parent
                (treesit-buffer-root-node))))
    (if (treesit-search-subtree (treesit-buffer-root-node) "^ERROR$")
        ;; If we have syntax errors, assume that functions will have ends when entering
        ;; matlab-ts-mode, otherwise leave matlab-ts--function-indent-level unchanged.
        (when (equal matlab-ts-mode--function-indent-level 'unset)
          (setq-local matlab-ts-mode--function-indent-level matlab-ts-mode--indent-level))
      (let ((first-fcn (treesit-search-subtree root (rx bol "function_definition" eol))))
        (if (not first-fcn)
            ;; assume that if functions are added they will have ends
            (setq-local matlab-ts--function-indent-level t)
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

(defun matlab-ts-mode--prev-real-line (_n _p bol &rest _)
  "Return point of first non-whitespace looking backward.
BOL, beginning-of-line point, is where to start from."
  (save-excursion
    (goto-char bol)
    (forward-line -1)
    (while (and (not (bobp))
                (looking-at "^[ \t]*$"))
      (forward-line -1))
    (skip-chars-forward " \t")
    (point)))

(defun matlab-ts-mode--prev-real-line-is (node-type prev-real-line-node-type)
  "Node type matcher and previous real line type matcher.
Returns non-nil if the current tree-sitter node matches NODE-TYPE and
the previous non-empty line tree-sitter node type matches
PREV-REAL-LINE-NODE-TYPE.  NODE-TYPE can be nil when there's no current
node or a regular expression.  PREV-REAL-LINE-NODE-TYPE is a regular
expression."
  (lambda (node parent bol &rest _)
    (when (or (and (not node-type)
                   (not node))
              (and node-type
                   (string-match-p node-type (or (treesit-node-type node) ""))))
      (let* ((prev-real-line-bol (matlab-ts-mode--prev-real-line node parent bol))
             (p-node (treesit-node-at prev-real-line-bol)))
        (string-match-p prev-real-line-node-type (or (treesit-node-type p-node) ""))))))

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

(defvar matlab-ts-mode--indent-rules
  `((matlab

     ;; I-Rule: classdef's, function's, or code for a script that is at the top-level
     ((parent-is ,(rx bol "source_file" eol)) column-0 0)

     ;; I-Rule: within a function/classdef doc block comment "%{ ... %}"?
     ((lambda (node parent bol &rest _)
        (and (not node)
             (string= "comment" (treesit-node-type parent))
             (not (save-excursion (goto-char bol)
                                  (looking-at "%")))
             (matlab-ts-mode--is-doc-comment parent (treesit-node-parent parent))))
      parent 2)

     ;; I-Rule: function/classdef doc comment?
     ((lambda (node parent &rest _)
        (or (and (string= "comment" (or (treesit-node-type node) ""))
                 (matlab-ts-mode--is-doc-comment node parent))
            (and (not node)
                 (string= "comment" (treesit-node-type parent))
                 (matlab-ts-mode--is-doc-comment parent (treesit-node-parent parent)))))
      parent 0)

     ;; I-Rule: within a code block comment "%{ ... %}"?
     ((lambda (node parent bol &rest _)
        (and (not node)
             (string= "comment" (treesit-node-type parent))
             (not (save-excursion (goto-char bol)
                                  (looking-at "%")))))
      parent
      2)

     ;; I-Rule: last line of code block coment "%{ ... %}"?
     ((lambda (node parent bol &rest _)
        (and (not node)
             (string= "comment" (treesit-node-type parent))
             (save-excursion (goto-char bol)
                             (looking-at "%"))))
      parent
      0)

     ;; I-Rule: switch case and otherwise statements
     ((node-is ,(rx bol (or "case_clause" "otherwise_clause") eol))
      parent ,matlab-ts-mode--switch-indent-level)

     ;; I-Rule: first line of code witin a switch case or otherwise statement, node is block
     ((parent-is ,(rx bol (or "case_clause" "otherwise_clause") eol))
      parent ,matlab-ts-mode--switch-indent-level)

     ;; I-Rule: nested functions
     ((n-p-gp ,(rx bol "function_definition" eol)
              ,(rx bol "block" eol)
              ,(rx bol "function_definition" eol))
      parent 0)

     ;; I-Rule: elseif, else, catch, end statements go back to parent level
     ((node-is ,(rx bol (or "elseif_clause" "else_clause" "catch_clause" "end") eol)) parent 0)

     ;; I-Rule: function's
     ((parent-is ,(rx bol "function_definition" eol))
      parent ,#'matlab-ts-mode--set-function-indent-level)

     ;; I-Rule: constructs within classdef or function's.
     ((node-is ,(rx bol (or "arguments_statement" "block" "enumeration" "enum" "methods" "events"
                            "function_definition" "property" "properties")
                    eol))
      parent ,matlab-ts-mode--indent-level)

     ;; I-Rule: code in if, for, methods, arguments statements, etc.
     ((parent-is ,(rx bol (or "if_statement" "for_statement" "while_statement"
                              "methods" "events" "enumeration"
                              "function_definition" "arguments_statement")
                      eol))
      parent ,matlab-ts-mode--indent-level)

     ;; I-Rule: function a<RET>
     ;;         end
     ((n-p-gp nil ,(rx bol "\n" eol) ,(rx bol (or "function_definition") eol))
      grand-parent ,matlab-ts-mode--indent-level)

     ;; I-Rule: case 10<RET>
     ((n-p-gp nil ,(rx bol "\n" eol) ,(rx bol (or "switch_statement" "case_clause"
                                                  "otherwise_clause")
                                          eol))
      grand-parent ,matlab-ts-mode--switch-indent-level)

     ;; I-Rule:  if condition1 || ...     |      if condition1 + condition2 == ...
     ;; <TAB>       condition2 || ...     |         2770000 ...
     ((parent-is ,(rx bol (or "boolean_operator" "comparison_operator") eol)) parent 0)

     ;; I-Rule:  elseif ...
     ;; <TAB>        condition2 || ...
     ((parent-is ,(rx bol (or "else_clause" "elseif_clause") eol))
      parent ,matlab-ts-mode--indent-level)

     ;; I-Rule: if a ...
     ;; <TAB>
     ((n-p-gp nil ,(rx bol "\n" eol)
              ,(rx bol (or "if_statement" "else_clause" "elseif_clause" ) eol))
      grand-parent ,matlab-ts-mode--indent-level)

     ;; I-Rule: disp(myMatrix(1:  ...
     ;; <TAB>                 end));
     ((parent-is ,(rx bol "range" eol)) parent 0)

     ;; I-Rule: try<RET>    |   catch<RET>
     ((parent-is ,(rx bol (or "try_statement" "catch_clause") eol))
      parent ,matlab-ts-mode--indent-level)

     ;; I-Rule: function a
     ;;             x = 1;
     ;; <TAB>       y = 2;
     ((parent-is ,(rx bol "block" eol)) parent 0)

     ;; I-Rule: "switch var" and we type RET after the var
     (,(matlab-ts-mode--prev-real-line-is (rx bol "\n" eol) (rx bol "switch" eol))
      ,#'matlab-ts-mode--prev-real-line ,matlab-ts-mode--switch-indent-level)

     ;; I-Rule: "function foo()" and we type RET after the ")"
     (,(matlab-ts-mode--prev-real-line-is nil (rx bol "function" eol))
      ,#'matlab-ts-mode--prev-real-line ,matlab-ts-mode--indent-level)

     ;; I-Rule:  a = ...
     ;; <TAB>        1;
     ((parent-is ,(rx bol "assignment" eol)) parent ,matlab-ts-mode--indent-level)

     ;; I-Rule:  a = 2 * ...
     ;; <TAB>        1;
     ((parent-is ,(rx bol "binary_operator" eol)) parent 0)

     ;; I-Rule:  a = ( ...       |     a = [  ...     |     a = {    ...
     ;;               1 ...      |          1 ...     |            1 ...
     ;; <TAB>        );          |         ];         |         };
     ((node-is ,(rx bol (or ")" "]" "}") eol)) parent 0)

     ;; I-Rule:  a = ( ...
     ;; <TAB>         1 ...
     ((parent-is ,(rx bol "parenthesis" eol)) parent 1)

     ;; I-Rule:  a = [   ...    |    a = { ...
     ;; <TAB>          2 ...    |          2 ...
     ((parent-is ,(rx bol (or "matrix" "cell") eol)) parent ,matlab-ts-mode--array-indent-level)

     ;; I-Rule:  function [   ...              |    function name (   ...
     ;; <TAB>              a, ... % comment    |                   a, ... % comment
     ((parent-is ,(rx bol (or "multioutput_variable" "function_arguments") eol)) parent 1)

     ;; I-Rule:  a = [    2 ...       |   function a ...
     ;; <TAB>             1 ...       |            = fcn
     ((parent-is ,(rx bol (or "row" "function_output") eol)) parent 0)

     ;; I-Rule:  a = ...
     ;; <TAB>        1;
     ((n-p-gp nil nil ,(rx bol "assignment" eol)) grand-parent ,matlab-ts-mode--indent-level)

     ;; I-Rule:  a = my_function(1, ...
     ;; <TAB>                    2, ...
     ((parent-is ,(rx bol "arguments" eol)) parent 0)

     ;; I-Rule:  my_function( ...
     ;; <TAB>        1, ...
     ((node-is ,(rx bol "arguments" eol)) parent ,matlab-ts-mode--indent-level)

     ;; I-Rule: function indent_tab_between_fcns   |   function indent_tab_in_fcn
     ;;         end                                |      disp('here')
     ;; <TAB>                                      |
     ;;         function b                         |   end
     ;;         end                                |
     ((lambda (node parent bol)
        (and (not node)
             (string= (treesit-node-type parent) "\n")))
      grand-parent 0)

     ;; I-Rule: In an empty line, string, etc. just maintain indent
     ;;         switch in
     ;;           case 10
     ;;             disp('11');
     ;; <TAB>
     (no-node ,#'matlab-ts-mode--prev-real-line 0)

     ;; I-Rule: Assert if no rule matched and asserts are enabled.
     ,matlab-ts-mode--indent-assert-rule
     ))
  "Tree-sitter indent rules for `matlab-ts-mode'.")

;;; Movement

(defvar matlab-ts-mode--thing-settings
  `((matlab
     (defun ,(rx bol "function_definition" eol)))
    ;; Future possibility, setup sexp, text, sentance.
    ;; - Setup sexp, text for C-M-f C-M-b, but how do we define balanced expressions?
    ;;   Note setting text alters code path for M-; (comment-dwim), but shouldn't
    ;;   change behavior.
    ;; - Setup sentance for M-e (forward-sentance), but how do we define a sentance?
    ;;
    ;; See Perl's sexp setup, https://hg.sr.ht/~pranshu/perl-ts-mode
    ;;   Sexp navigation
    ;;   my $r = join '-', @arr + 2 - 1;
    ;;   The sexps (circle bracket), and sentence[square bracker] would be:
    ;;
    ;;   [((my) ($r)) = ((join) '-', (((@arr) + (2)) - (1)));]
    ;;   Commands like C-M-b, C-M-f, M-e, M-a will reflect on this.
    ;;   It makes moving around seem more fun and cool.
    )
  "Tree-sitter things for movement.")

;;; Change Log

(defun matlab-ts-mode--defun-name (node)
  "Return the defun name of NODE for Change Log entries."
    (when (string-match-p
           (rx bol (or "function_definition" "class_definition") eol)
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
                         (string-match-p (rx bol (or "get." "set.") eol) prev-type))
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
    (or (string-match-p (rx bol (or "function_definition" "class_definition") eol) node-type)
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
                  ((string-match-p (rx bol (or "function_definition" "class_definition") eol)
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
       ((string-match-p (rx bol (or "comment" "line_continuation") eol) type-back1)
        (save-excursion
          (forward-char -1)
          (looking-at "\\w\\|\\s_\\|\\.")))

       ;; Case: string delimiter
       ;;    double up if starting a new string => return nil
       ((string= "'" type-back1)
        (not (string= "string" (treesit-node-type (treesit-node-parent node-back1)))))

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

  (when (treesit-ready-p 'matlab)
    (treesit-parser-create 'matlab)

    ;; Syntax table - think of this as a "language character descriptor". It tells us what
    ;; characters belong to word like things giving us movement commands e.g. C-M-f, matching
    ;; parens, `show-paren-mode', etc.
    ;; See: ./tests/test-matlab-ts-mode-syntax-table.el
    (set-syntax-table matlab-ts-mode--syntax-table)
    (setq-local syntax-propertize-function #'matlab-ts-mode--syntax-propertize)

    ;; Comments
    ;; See: tests/test-matlab-ts-mode-comments.el
    (setq-local comment-start      "% ")
    (setq-local comment-end        "")
    (setq-local comment-start-skip "%\\s-+")

    ;; Setup `forward-page' and `backward-page' to use ^L or "%% heading" comments
    ;; See: ./tests/test-matlab-ts-mode-page.el
    (setq-local page-delimiter "^\\(?:\f\\|%%\\(?:\\s-\\|\n\\)\\)")

    ;; Font-lock. See: ./tests/test-matlab-ts-mode-font-lock.el
    (setq-local treesit-font-lock-level matlab-ts-mode-font-lock-level)
    (setq-local treesit-font-lock-settings matlab-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list '((comment definition)
                                                 (keyword string type)
                                                 (number bracket delimiter)
                                                 (syntax-error)))

    ;; Indent. See: ./tests/test-matlab-ts-mode-indent.el
    (matlab-ts-mode--set-function-indent-level)
    (setq-local indent-tabs-mode nil) ;; for consistency between Unix and Windows we don't use TABs.
    (setq-local treesit-simple-indent-rules
                (if treesit--indent-verbose ;; add debugging print as first rule?
                    (list (append `,(list (caar matlab-ts-mode--indent-rules))
                                  (list matlab-ts--indent-debug-rule)
                                  (cdar matlab-ts-mode--indent-rules)))
                  matlab-ts-mode--indent-rules))

    ;; Movement. See: tests/test-matlab-ts-mode-movement.el
    (setq-local treesit-thing-settings matlab-ts-mode--thing-settings)

    ;; Change Logs. See: tests/test-matlab-ts-mode-treesit-defun-name.el
    (setq-local treesit-defun-name-function #'matlab-ts-mode--defun-name)

    ;; M-x imenu
    ;; See: ./tests/test-matlab-ts-mode-imenu.el
    ;;
    ;; TODO - lsp-mode and imenu
    ;; I think we need (setq-local lsp-enable-imenu nil) when lsp-mode is used.  Can we find a
    ;; automatic way to do this? See:
    ;; https://www.reddit.com/r/emacs/comments/1c216kr/experimenting_with_tree_sitter_and_imenulist/
    (setq-local imenu-create-index-function #'matlab-ts-mode--imenu-create-index)

    ;; M-x outline-minor-mode
    ;; See: ./tests/test-matlab-ts-mode-outline.el
    (setq-local treesit-outline-predicate #'matlab-ts-mode--outline-predicate)

    ;; Save hook. See: ./tests/test-matlab-ts-mode-on-save-fixes.el
    (add-hook 'write-contents-functions #'matlab-ts-mode--write-file-callback)

    ;; Electric pair mode. See tests/test-matlab-ts-mode-electric-pair.el
    (setq-local electric-pair-inhibit-predicate #'matlab-ts-mode--electric-pair-inhibit-predicate)

    ;; TODO Highlight parens OR if/end type blocks
    ;; TODO font-lock highlight operators, *, /, +, -, ./, booleans true/false, etc.
    ;; TODO face for all built-in functions such as dbstop, quit, sin, etc.
    ;;   https://www.mathworks.com/help/matlab/referencelist.html?type=function&category=index&s_tid=CRUX_lftnav_function_index
    ;;   https://stackoverflow.com/questions/51942464/programmatically-return-a-list-of-all-functions/51946257
    ;;   Maybe use completion api and complete on each letter?
    ;;   Maybe look at functionSignatures.json?
    ;; TODO code folding

    (treesit-major-mode-setup)))

(provide 'matlab-ts-mode)
;;; matlab-ts-mode.el ends here
