% -*- matlab-ts -*- %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>

% Here MATLAB indent "SomeLongString" to be 4 from the '(' in "someFunction1(" %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>
% This differs from clang-format on C code where it will produce: %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher parent 0)}>
%    something.foo = someFunction1(someFunction2( %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher parent 0)}>
%        "SomeLongString")); %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher parent 0)}>
% %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher parent 0)}>
something.foo = someFunction1(someFunction2( ... %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>
                                  "SomeLongString")); %  <{Matched rule: ((node-is "\\`arguments\\'") parent 4)}>

% Here MATLAB indents similar to the prior. %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>
% clang-format C also indents this way: %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher parent 0)}>
%    something.foo = someFunction1(someFunction2( %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher parent 0)}>
%                                      "SomeLongString"), %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher parent 0)}>
%                                  arg2, %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher parent 0)}>
%                                  arg3); %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher parent 0)}>
% %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher parent 0)}>
something.foo = someFunction1(someFunction2( ... %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>
                                  "SomeLongString"), ... %  <{Matched rule: ((node-is "\\`arguments\\'") parent 4)}>
                              arg2, ... %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
                              arg3); %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>

% Here MATLAB indents similar to the prior. %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>
% This differs from clang-format on C code where it will produce: %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher parent 0)}>
%    something.foo = someFunction1(someFunction2( %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher parent 0)}>
%        "SomeLongString", %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher parent 0)}>
%        arg2, %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher parent 0)}>
%        arg3)); %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher parent 0)}>
something.foo = someFunction1(someFunction2( ... %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>
                                  "SomeLongString", ... %  <{Matched rule: ((node-is "\\`arguments\\'") parent 4)}>
                                  arg2, ... %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
                                  arg3)); %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>

% *** Conclusion *** %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>
%   For consistency, MATLAB is correct. Adding an argument to a function shouldn't cause %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher parent 0)}>
%   indent differences. This is likely a miss on clang-format indent rules part. %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher parent 0)}>


% --- Test unnecessary spaces --- %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>

something.foo = someFunction1  (someFunction2  ( ... %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>
                                    "SomeLongString")); %  <{Matched rule: ((node-is "\\`arguments\\'") parent 4)}>

something.foo = someFunction1  (someFunction2  ( ... %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>
                                    "SomeLongString"), ... %  <{Matched rule: ((node-is "\\`arguments\\'") parent 4)}>
                                arg2, ... %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
                                arg3); %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>

something.foo = someFunction1  (someFunction2  ( ... %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>
                                    "SomeLongString", ... %  <{Matched rule: ((node-is "\\`arguments\\'") parent 4)}>
                                    arg2, ... %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
                                    arg3)); %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>

% --- Test namespaces --- %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>

something.foo = fooNamespace.someFunction1(fooNamespace.someFunction2( ... %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>
                                               "SomeLongString")); %  <{Matched rule: ((n-p-gp "\\`arguments\\'" "\\`function_call\\'" "\\`field_expression\\'") grand-parent 4)}>

something.foo = fooNamespace.someFunction1(fooNamespace.someFunction2( ... %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>
                                               "SomeLongString"), ... %  <{Matched rule: ((n-p-gp "\\`arguments\\'" "\\`function_call\\'" "\\`field_expression\\'") grand-parent 4)}>
                                           arg2, ... %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
                                           arg3); %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>

something.foo = fooNamespace.someFunction1(fooNamespace.someFunction2( ... %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>
                                               "SomeLongString", ... %  <{Matched rule: ((n-p-gp "\\`arguments\\'" "\\`function_call\\'" "\\`field_expression\\'") grand-parent 4)}>
                                               arg2, ... %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
                                               arg3)); %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>

% --- Test namespaces with unnecessary spaces %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>

something.foo = fooNamespace.someFunction1  (fooNamespace.someFunction2  ( ... %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>
                                                 "SomeLongString")); %  <{Matched rule: ((n-p-gp "\\`arguments\\'" "\\`function_call\\'" "\\`field_expression\\'") grand-parent 4)}>

something.foo = fooNamespace.someFunction1  (fooNamespace.someFunction2  ( ... %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>
                                                 "SomeLongString"), ... %  <{Matched rule: ((n-p-gp "\\`arguments\\'" "\\`function_call\\'" "\\`field_expression\\'") grand-parent 4)}>
                                             arg2, ... %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
                                             arg3); %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>

something.foo = fooNamespace.someFunction1  (fooNamespace.someFunction2  ( ... %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>
                                                 "SomeLongString", ... %  <{Matched rule: ((n-p-gp "\\`arguments\\'" "\\`function_call\\'" "\\`field_expression\\'") grand-parent 4)}>
                                                 arg2, ... %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
                                                 arg3)); %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
