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
  ;; correctly handles them by letting use look for these as content of the nodes.
  '(
      "arguments"
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
      "spmd"
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

(defun matlab-ts-mode--doc-comment-capture (comment-node override start end &rest _)
  "Fontify function/classdef documentation comments.
In MATLAB,
  function out = myFunction
  % The documentation help comment for myFunction immediately follows the
  % function defintion.

      % code comment have a blank line
      out = 1;
  end

  function out = myFunctionWithoutHelp

      % code comment have a blank line
      out = 1;
  end

COMMENT-NODE is the tree-sitter node from the \"doc comments\"
treesit-font-lock-rules rule and OVERRIDE is from that rule.
START and END specify the region to be fontified."
  (let* ((prev-node (treesit-node-prev-sibling comment-node))
         (prev-node-type (treesit-node-type prev-node))
         (prev2-node (when (string= prev-node-type "identifier")
                       (treesit-node-prev-sibling prev-node)))
         (real-prev-node-type (if prev2-node (treesit-node-type prev2-node) prev-node-type))
         (real-prev-node (or prev2-node prev-node))
         (is-doc-comment-candidate
          (or (string= real-prev-node-type "function_arguments") ;; function foo(in)
              (string= real-prev-node-type "function_output")    ;; function out = foo
              (string= real-prev-node-type "function")           ;; function foo
              (string= real-prev-node-type "classdef")           ;; classdef foo
              (string= real-prev-node-type "superclasses")))     ;; classdef foo < ParentClass
         (is-doc-comment (and is-doc-comment-candidate
                              (save-excursion
                                (goto-char (treesit-node-start comment-node))
                                (not (re-search-backward "^[ \t]*$"
                                                         (treesit-node-end real-prev-node)
                                                         t))))))
    (when is-doc-comment
      (treesit-fontify-with-override
       (treesit-node-start comment-node) (treesit-node-end comment-node)
       font-lock-doc-face override start end))))

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

   ;; Keywords: if, else, etc.
   :language 'matlab
   :feature 'keyword
   `(([,@matlab-ts-mode--keywords] @font-lock-keyword-face))

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
     ;; Function single output arugment: function out = functionName(in1, in2)
     (function_output (identifier) @font-lock-variable-name-face)
     ;; Function multiple ouptut arguments: function [out1, out2] = functionName(in1, in2)
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

;;;###autoload
(define-derived-mode matlab-ts-mode prog-mode "MATLAB:ts"
  "Major mode for editing MATLAB files with tree-sitter."

  (when (treesit-ready-p 'matlab)
    (treesit-parser-create 'matlab)

    ;; Font-lock
    (setq-local treesit-font-lock-settings matlab-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '((comment definition)
                  (keyword string type)
                  (number)
                  (bracket delimiter error)))

    (treesit-major-mode-setup)))

(provide 'matlab-ts-mode)
;;; matlab-ts-mode.el ends here
