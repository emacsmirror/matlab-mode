% -*- matlab-ts -*- %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

% Here MATLAB indent "SomeLongString" to be 4 from the '(' in "someFunction1(" %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
% This differs from clang-format on C code where it will produce: %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher matlab-ts-mode--i-block-comment-end-anchor 0)}>
%    something.foo = someFunction1(someFunction2( %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher matlab-ts-mode--i-block-comment-end-anchor 0)}>
%        "SomeLongString")); %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher matlab-ts-mode--i-block-comment-end-anchor 0)}>
% %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher matlab-ts-mode--i-block-comment-end-anchor 0)}>
something.foo = someFunction1(someFunction2( ... %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
                                  "SomeLongString")); %  <{Matched rule: ((node-is "\\`arguments\\'") parent 4)}>

% Here MATLAB indents similar to the prior. %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
% clang-format C also indents this way: %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher matlab-ts-mode--i-block-comment-end-anchor 0)}>
%    something.foo = someFunction1(someFunction2( %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher matlab-ts-mode--i-block-comment-end-anchor 0)}>
%                                      "SomeLongString"), %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher matlab-ts-mode--i-block-comment-end-anchor 0)}>
%                                  arg2, %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher matlab-ts-mode--i-block-comment-end-anchor 0)}>
%                                  arg3); %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher matlab-ts-mode--i-block-comment-end-anchor 0)}>
% %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher matlab-ts-mode--i-block-comment-end-anchor 0)}>
something.foo = someFunction1(someFunction2( ... %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
                                  "SomeLongString"), ... %  <{Matched rule: ((node-is "\\`arguments\\'") parent 4)}>
                              arg2, ... %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
                              arg3); %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>

% Here MATLAB indents similar to the prior. %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
% This differs from clang-format on C code where it will produce: %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher matlab-ts-mode--i-block-comment-end-anchor 0)}>
%    something.foo = someFunction1(someFunction2( %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher matlab-ts-mode--i-block-comment-end-anchor 0)}>
%        "SomeLongString", %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher matlab-ts-mode--i-block-comment-end-anchor 0)}>
%        arg2, %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher matlab-ts-mode--i-block-comment-end-anchor 0)}>
%        arg3)); %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher matlab-ts-mode--i-block-comment-end-anchor 0)}>
something.foo = someFunction1(someFunction2( ... %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
                                  "SomeLongString", ... %  <{Matched rule: ((node-is "\\`arguments\\'") parent 4)}>
                                  arg2, ... %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
                                  arg3)); %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>

% *** Conclusion *** %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
%   For consistency, MATLAB is correct. Adding an argument to a function shouldn't cause %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher matlab-ts-mode--i-block-comment-end-anchor 0)}>
%   indent differences. This is likely a miss on clang-format indent rules part. %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher matlab-ts-mode--i-block-comment-end-anchor 0)}>


% --- Test unnecessary spaces --- %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

something.foo = someFunction1  (someFunction2  ( ... %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
                                    "SomeLongString")); %  <{Matched rule: ((node-is "\\`arguments\\'") parent 4)}>

something.foo = someFunction1  (someFunction2  ( ... %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
                                    "SomeLongString"), ... %  <{Matched rule: ((node-is "\\`arguments\\'") parent 4)}>
                                arg2, ... %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
                                arg3); %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>

something.foo = someFunction1  (someFunction2  ( ... %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
                                    "SomeLongString", ... %  <{Matched rule: ((node-is "\\`arguments\\'") parent 4)}>
                                    arg2, ... %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
                                    arg3)); %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>

% --- Test namespaces --- %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

something.foo = fooNamespace.someFunction1(fooNamespace.someFunction2( ... %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
                                               "SomeLongString")); %  <{Matched rule: ((n-p-gp "\\`arguments\\'" "\\`function_call\\'" "\\`field_expression\\'") grand-parent 4)}>

something.foo = fooNamespace.someFunction1(fooNamespace.someFunction2( ... %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
                                               "SomeLongString"), ... %  <{Matched rule: ((n-p-gp "\\`arguments\\'" "\\`function_call\\'" "\\`field_expression\\'") grand-parent 4)}>
                                           arg2, ... %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
                                           arg3); %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>

something.foo = fooNamespace.someFunction1(fooNamespace.someFunction2( ... %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
                                               "SomeLongString", ... %  <{Matched rule: ((n-p-gp "\\`arguments\\'" "\\`function_call\\'" "\\`field_expression\\'") grand-parent 4)}>
                                               arg2, ... %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
                                               arg3)); %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>

% --- Test namespaces with unnecessary spaces %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

something.foo = fooNamespace.someFunction1  (fooNamespace.someFunction2  ( ... %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
                                                 "SomeLongString")); %  <{Matched rule: ((n-p-gp "\\`arguments\\'" "\\`function_call\\'" "\\`field_expression\\'") grand-parent 4)}>

something.foo = fooNamespace.someFunction1  (fooNamespace.someFunction2  ( ... %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
                                                 "SomeLongString"), ... %  <{Matched rule: ((n-p-gp "\\`arguments\\'" "\\`function_call\\'" "\\`field_expression\\'") grand-parent 4)}>
                                             arg2, ... %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
                                             arg3); %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>

something.foo = fooNamespace.someFunction1  (fooNamespace.someFunction2  ( ... %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
                                                 "SomeLongString", ... %  <{Matched rule: ((n-p-gp "\\`arguments\\'" "\\`function_call\\'" "\\`field_expression\\'") grand-parent 4)}>
                                                 arg2, ... %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
                                                 arg3)); %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
