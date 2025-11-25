% -*- matlab-ts -*- %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
% %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher parent 0)}>
% t-utils-test-indent: no-line-by-line-indent - typing the incomplete expression line-by-line %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher parent 0)}>
% results in a parse tree that can't match below. %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher parent 0)}>
% %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher parent 0)}>
function a=indent_cont_statement_without_ellipsis %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
% The matlab-tree-sitter, by design for simplicity treats "..." as comments so the following %  <{Matched rule: (matlab-ts-mode--i-doc-comment-matcher matlab-ts-mode--i-doc-comment-anchor matlab-ts-mode--i-doc-comment-offset)}>
% is an error in MATLAB, but doesn't generate a parse error and indents the same as if %  <{Matched rule: (matlab-ts-mode--i-doc-comment-matcher matlab-ts-mode--i-doc-comment-anchor matlab-ts-mode--i-doc-comment-offset)}>
% the ellipsis (...) were present. %  <{Matched rule: (matlab-ts-mode--i-doc-comment-matcher matlab-ts-mode--i-doc-comment-anchor matlab-ts-mode--i-doc-comment-offset)}>
    a = %  <{Matched rule: ((parent-is "\\`function_definition\\'") parent matlab-ts-mode--set-function-indent-level-for-gp)}>
        1 + %  <{Matched rule: ((parent-is "\\`assignment\\'") parent 4)}>
        2; %  <{Matched rule: ((parent-is "\\`binary_operator\\'") parent 0)}>
end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
