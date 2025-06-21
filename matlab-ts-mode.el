;;; matlab-ts-mode.el --- MATLAB Tree-Sitter Mode -*- lexical-binding: t -*-

;; Copyright 2025 Free Software Foundation, Inc.
;;
;; URL: https://github.com/mathworks/Emacs-MATLAB-Mode
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Author: John Ciolfi <john.ciolfi.32@gmail.com>
;; Created: Jun-14-2025
;; Keywords: MATLAB(R)
;; Package-Requires: ((emacs "30.1"))

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

;;-------------------------;;
;; Section: Customizations ;;
;;-------------------------;;

(defgroup matlab-ts nil
  "MATLAB(R) tree-sitter mode."
  :prefix "matlab-ts-"
  :group 'languages)

(defface matlab-ts-pragma-face
  '((t :inherit font-lock-comment-face
       :bold t))
  "*Face to use for pragma %# lines.")

(defface matlab-ts-string-delimiter-face
  '((t :inherit font-lock-string-face
       :bold t))
  "*Face to use for \\='single quote\\=' and \"double quote\" string delimiters.")

(defface matlab-ts-comment-heading-face
  '((t :inherit font-lock-comment-face
       :overline t
       :bold t))
  "Face for \"%% code section\" headings when NOT in matlab-sections-minor-mode.")

(defcustom matlab-ts-font-lock-level 4
  "Level of font lock, 1 for minimum syntax highlighting and 4 for maximum."
  :type '(choice (const :tag "Minimal" 1)
		 (const :tag "Low" 2)
		 (const :tag "Standard" 3)
		 (const :tag "Standard plus parse errors" 4)))

;;--------------------;;
;; Section: font-lock ;;
;;--------------------;;

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
    "global"
    "if"
    "methods"
    "otherwise"
    "parfor"
    "persistent"
    "properties"
    (return_statement)
    "switch"
    "try"
    "while")
  "MATLAB keywords for tree-sitter font-locking.")

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
  "MATLAB data type functions.")

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

  (when (string-match-p (rx bol (or "function_definition" "class_definition") eol)
                        (treesit-node-type parent))
    (let ((definition-point (treesit-node-start parent)))
      (save-excursion
        (goto-char (treesit-node-start comment-node))
        (beginning-of-line)

        ;; Skip backwards over the copyright line to prior content.
        (when (looking-at "^[ \t]*%[ \t]*copyright\\b")
          (beginning-of-line)
          (forward-line -1)
          (while (looking-at "^[ \t]*$")
            (forward-line -1)))

        ;; result - is doc comment?
        (or (<= (point) definition-point) ;; at definition?
            (and (> (point) definition-point)
                 (not (re-search-backward "^[ \t]*$" definition-point t))))))))

(defun matlab-ts-mode--doc-comment-capture (comment-node override start end &rest _)
  "Fontify function/classdef documentation comments.
COMMENT-NODE is the tree-sitter node from the \"doc comments\"
treesit-font-lock-rules rule and OVERRIDE is from that rule.
START and END specify the region to be fontified."
  (when (matlab-ts-mode--is-doc-comment comment-node (treesit-node-parent comment-node))
    (treesit-fontify-with-override
     (treesit-node-start comment-node) (treesit-node-end comment-node)
     font-lock-doc-face override start end)))

(defvar matlab-ts-mode--font-lock-settings
  (treesit-font-lock-rules

   ;; Comments and line continuation: ... optional text
   :language 'matlab
   :feature 'comment
   '((comment) @font-lock-comment-face
     (line_continuation) @font-lock-comment-face)

   ;; Special comments that override normal comment font
   :language 'matlab
   :feature 'comment
   :override t
   '(((comment) @matlab-ts-pragma-face (:match "^%#.+$" @matlab-ts-pragma-face)) ;; %#pragma's
     ((comment) @matlab-ts-comment-heading-face ;; %% comment heading
      (:match "^%%\\(?:[ \t].+\\)?$" @matlab-ts-comment-heading-face))
     (function_definition (comment) @matlab-ts-mode--doc-comment-capture) ;; doc help comments
     (class_definition (comment) @matlab-ts-mode--doc-comment-capture)) ;; doc help comments

   ;; ;; Keywords: if, else, etc.
   :language 'matlab
   :feature 'keyword
   `([,@matlab-ts-mode--keywords] @font-lock-keyword-face)

   ;; function/classdef
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

   ;; Strings
   :language 'matlab
   :feature 'string
   '((string_content) @font-lock-string-face
     ((string_content) ["\"" "'"]) @matlab-ts-string-delimiter-face
     (string ["\"" "'"] @matlab-ts-string-delimiter-face))

   ;; Transpose uses "'" after an identifier, e.g. for matrix A we tranpose it via: A' since "'" is
   ;; also used as a string, we use a different face for transpose and put it under the string
   ;; category.
   :language 'matlab
   :feature 'string
   '((postfix_operator "'" @font-lock-function-name-face))

   ;; Types, e.g. int32()
   :language 'matlab
   :feature 'type
   `((function_call name: (identifier)
                    @font-lock-type-face
                    (:match ,(rx-to-string
                              `(seq bol
                                    (or ,@matlab-ts-mode--type-functions)
                                    eol))
                              @font-lock-type-face)))

   ;; Constant numbers
   :language 'matlab
   :feature 'number
   '((number) @font-lock-constant-face)

   ;; Brackets
   :language 'matlab
   :feature 'bracket
   '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

   ;; Delimiters
   :language 'matlab
   :feature 'delimiter
   '((["," "." ";" ":" "@" "?"]) @font-lock-delimiter-face)

   ;; Syntax errors
   :language 'matlab
   :feature 'error
   :override t
   '((ERROR) @font-lock-warning-face)

   )
  "MATLAB tree-sitter font-lock settings.")


;;-----------------:;
;; Section: Indent ;;
;;-----------------;;

;; We discourage customizing the indentation rules. Having one-style of consistent indentation makes
;; reading others' code easier.
(defvar matlab-ts-mode--indent-level 4
  "Indentation level.")
(defvar matlab-ts-mode--switch-indent-level (/ matlab-ts-mode--indent-level 2)
  "Indentation level for switch-case statements.")
(defvar matlab-ts-mode--array-indent-level 2
  "Indentation level for elements in an array.")

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

(defvar tmp-debug-indent-rule
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

     ;; ,tmp-debug-indent-rule

     ;; Rule: classdef's, function's, or code for a script that is at the top-level
     ((parent-is ,(rx bol "source_file" eol)) column-0 0)

     ;; Rule: within a function/classdef doc block comment "%{ ... %}"?
     ((lambda (node parent bol &rest _)
        (and (not node)
             (string= "comment" (treesit-node-type parent))
             (not (save-excursion (goto-char bol)
                                  (looking-at "%")))
             (matlab-ts-mode--is-doc-comment parent (treesit-node-parent parent))))
      parent 2)

     ;; Rule: function/classdef doc comment?
     ((lambda (node parent &rest _)
        (or (and (string= "comment" (or (treesit-node-type node) ""))
                 (matlab-ts-mode--is-doc-comment node parent))
            (and (not node)
                 (string= "comment" (treesit-node-type parent))
                 (matlab-ts-mode--is-doc-comment parent (treesit-node-parent parent)))))
      parent 0)

     ;; Rule: within a code block comment "%{ ... %}"?
     ((lambda (node parent bol &rest _)
        (and (not node)
             (string= "comment" (treesit-node-type parent))
             (not (save-excursion (goto-char bol)
                                  (looking-at "%")))))
      parent
      2)

     ;; Rule: last line of code block coment "%{ ... %}"?
     ((lambda (node parent bol &rest _)
        (and (not node)
             (string= "comment" (treesit-node-type parent))
             (save-excursion (goto-char bol)
                             (looking-at "%"))))
      parent
      0)

     ;; Rule: switch case and otherwise statements
     ((node-is ,(rx bol (or "case_clause" "otherwise_clause") eol))
      parent ,matlab-ts-mode--switch-indent-level)

     ;; Rule: first line of code witin a switch case or otherwise statement, node is block
     ((parent-is ,(rx bol (or "case_clause" "otherwise_clause") eol))
      parent ,matlab-ts-mode--switch-indent-level)

     ;; Rule: nested functions
     ((n-p-gp ,(rx bol "function_definition" eol) ,(rx bol "block" eol)
              ,(rx bol "function_definition" eol))
      parent 0)

     ;; Rule: constructs within classdef or function's.
     ((node-is ,(rx bol (or "arguments_statement" "block" "enumeration" "enum" "methods" "events"
                            "function_definition" "property" "properties")
                    eol))
      parent ,matlab-ts-mode--indent-level)

     ;; Rule: elseif, else, catch, end statements go back to parent level
     ((node-is ,(rx bol (or "elseif_clause" "else_clause" "catch_clause" "end") eol)) parent 0)

     ;; Rule: code in if, for, methods, function, arguments statements
     ((parent-is ,(rx bol (or "if_statement" "for_statement" "while_statement"
                              "methods" "events" "enumeration"
                              "function_definition" "arguments_statement")
                      eol))
      parent ,matlab-ts-mode--indent-level)

     ;; Rule: function a<RET>
     ;;       end
     ((n-p-gp nil ,(rx bol "\n" eol) ,(rx bol (or "function_definition") eol))
      grand-parent ,matlab-ts-mode--indent-level)

     ;; Rule: case 10<RET>
     ((n-p-gp nil ,(rx bol "\n" eol) ,(rx bol (or "switch_statement" "case_clause"
                                                  "otherwise_clause")
                                          eol))
      grand-parent ,matlab-ts-mode--switch-indent-level)

     ;; Rule:  if condition1 || ...     |      if condition1 + condition2 == ...
     ;; <TAB>     condition2 || ...     |         2770000 ...
     ((parent-is ,(rx bol (or "boolean_operator" "comparison_operator") eol)) parent 0)

     ;; Rule:  elseif ...
     ;; <TAB>      condition2 || ...
     ((parent-is ,(rx bol (or "else_clause" "elseif_clause") eol))
      parent ,matlab-ts-mode--indent-level)
     
     ;; Rule: if a ...
     ;; <TAB>
     ((n-p-gp nil ,(rx bol "\n" eol)
              ,(rx bol (or "if_statement" "else_clause" "elseif_clause" ) eol))
      grand-parent ,matlab-ts-mode--indent-level)

     ;; Rule: disp(myMatrix(1:  ...
     ;; <TAB>               end)); 
     ((parent-is ,(rx bol "range" eol)) parent 0)

     ;; Rule: try<RET>    |   catch<RET>
     ((parent-is ,(rx bol (or "try_statement" "catch_clause") eol))
      parent ,matlab-ts-mode--indent-level)

     ;; Rule: function a
     ;;           x = 1;
     ;; <TAB>     y = 2;
     ((parent-is ,(rx bol "block" eol)) parent 0)

     ;; Rule: "switch var" and we type RET after the var
     (,(matlab-ts-mode--prev-real-line-is (rx bol "\n" eol) (rx bol "switch" eol))
      ,#'matlab-ts-mode--prev-real-line ,matlab-ts-mode--switch-indent-level)

     ;; Rule: "function foo()" and we type RET after the ")"
     (,(matlab-ts-mode--prev-real-line-is nil (rx bol "function" eol))
      ,#'matlab-ts-mode--prev-real-line ,matlab-ts-mode--indent-level)

     ;; Rule:  a = ...
     ;; <TAB>      1;
     ((parent-is ,(rx bol "assignment" eol)) parent ,matlab-ts-mode--indent-level)

     ;; Rule:  a = 2 * ...
     ;; <TAB>      1;
     ((parent-is ,(rx bol "binary_operator" eol)) parent 0)

     ;; Rule:  a = ( ...       |     a = [  ...     |     a = {    ...
     ;;             1 ...      |          1 ...     |            1 ...
     ;; <TAB>      );          |         ];         |         };
     ((node-is ,(rx bol (or ")" "]" "}") eol)) parent 0)

     ;; Rule:  a = ( ...
     ;; <TAB>       1 ...
     ((parent-is ,(rx bol "parenthesis" eol)) parent 1)

     ;; Rule:  a = [   ...    |    a = { ...
     ;; <TAB>        2 ...    |          2 ...
     ((parent-is ,(rx bol (or "matrix" "cell") eol)) parent ,matlab-ts-mode--array-indent-level)

     ;; Rule:  function [   ...              |    function name (   ...
     ;; <TAB>            a, ... % comment    |                   a, ... % comment
     ((parent-is ,(rx bol (or "multioutput_variable" "function_arguments") eol)) parent 1)

     ;; Rule:  a = [    2 ...       |   function a ...
     ;; <TAB>           1 ...       |            = fcn
     ((parent-is ,(rx bol (or "row" "function_output") eol)) parent 0)

     ;; Rule:  a = ...
     ;; <TAB>      1;
     ((n-p-gp nil nil ,(rx bol "assignment" eol)) grand-parent ,matlab-ts-mode--indent-level)

     ;; Rule:  a = my_function(1, ...
     ;; <TAB>                  2, ...
     ((parent-is ,(rx bol "arguments" eol)) parent 0)

     ;; Rule:  my_function( ...
     ;; <TAB>      1, ...
     ((node-is ,(rx bol "arguments" eol)) parent ,matlab-ts-mode--indent-level)

     ;; Rule: function indent_tab_between_fcns   |   function indent_tab_in_fcn
     ;;       end                                |      disp('here')
     ;; <TAB>                                    |
     ;;       function b                         |   end
     ;;       end                                |
     ((lambda (node parent bol)
        (and (not node)
             (string= (treesit-node-type parent) "\n")))
      grand-parent 0)

     ;; Rule: In an empty line, string, etc. just maintain indent
     ;;       switch in
     ;;         case 10
     ;;           disp('11');
     ;; <TAB>
     (no-node ,#'matlab-ts-mode--prev-real-line 0)

     ;; Rule: Assert if no rule matched and asserts are enabled.
     ,matlab-ts-mode--indent-assert-rule
     ))
  "Tree-sitter indent rules for `matlab-ts-mode'.")

;;;###autoload
(define-derived-mode matlab-ts-mode prog-mode "MATLAB:ts"
  "Major mode for editing MATLAB files with tree-sitter."

  (when (treesit-ready-p 'matlab)
    (treesit-parser-create 'matlab)

    ;; Comments
    ;;   TODO: M-; on code comments, then a 2nd M-; doesn't uncomment
    ;;   likely need to also set up the syntax table.
    (setq-local comment-start      "%")
    (setq-local comment-end        "")
    (setq-local comment-start-skip "%\\s-+")

    ;; TODO function end handling
    ;; TODO add strings to syntax table?
    ;; TODO what about syntax table and electric keywords?
    ;; TODO function / end match like matlab-mode
    ;; TODO code folding
    ;; TODO fill paragraph, etc. look at c-ts-common.el
    ;; TODO outline: look at https://hg.sr.ht/~pranshu/perl-ts-mode/browse/perl-ts-mode.el?rev=tip
    ;; TODO imenu: look at https://hg.sr.ht/~pranshu/perl-ts-mode/browse/perl-ts-mode.el?rev=tip
    ;; TODO handle file name mismatch between function / classdef name
    ;; TODO face for all built-in functions such as dbstop, quit, sin, etc.
    ;;   https://www.mathworks.com/help/matlab/referencelist.html?type=function&category=index&s_tid=CRUX_lftnav_function_index
    ;;   https://stackoverflow.com/questions/51942464/programmatically-return-a-list-of-all-functions/51946257
    ;;   Maybe use completion api and complete on each letter?
    ;;   Maybe look at functionSignatures.json?

    ;; Font-lock
    (setq-local treesit-font-lock-level matlab-ts-font-lock-level)
    (setq-local treesit-font-lock-settings matlab-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '((comment definition)
                  (keyword string type)
                  (number bracket delimiter)
                  ( error)))

    ;; Indent
    (setq-local indent-tabs-mode nil ;; for consistency between Unix and Windows we don't use TABs.
                treesit-simple-indent-rules matlab-ts-mode--indent-rules)

    (treesit-major-mode-setup)))

(provide 'matlab-ts-mode)
;;; matlab-ts-mode.el ends here
