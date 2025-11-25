% -*- matlab-ts -*- %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

% t-utils-test-indent: no-line-by-line-indent - multiline comments cannot be indented correctly %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
% when typing line-by-line because we don't know where in a multiline comment until we see the end. %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher parent 0)}>

function b = indent_copyright_in_code %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
%{ %  <{Matched rule: (matlab-ts-mode--i-doc-comment-matcher matlab-ts-mode--i-doc-comment-anchor matlab-ts-mode--i-doc-comment-offset)}>
  sadf %  <{Matched rule: (matlab-ts-mode--i-in-block-comment-matcher parent 2)}>

  asdfasd %  <{Matched rule: (matlab-ts-mode--i-in-block-comment-matcher parent 2)}>
%} %  <{Matched rule: (matlab-ts-mode--i-doc-comment-matcher matlab-ts-mode--i-doc-comment-anchor matlab-ts-mode--i-doc-comment-offset)}>
    % foo %  <{Matched rule: ((parent-is "\\`function_definition\\'") parent matlab-ts-mode--set-function-indent-level-for-gp)}>
    % foo %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher parent 0)}>



    % copyright blah %  <{Matched rule: ((parent-is "\\`function_definition\\'") parent matlab-ts-mode--set-function-indent-level-for-gp)}>

    % foo %  <{Matched rule: ((parent-is "\\`function_definition\\'") parent matlab-ts-mode--set-function-indent-level-for-gp)}>
    b = 1; %  <{Matched rule: ((parent-is "\\`function_definition\\'") parent matlab-ts-mode--set-function-indent-level-for-gp)}>
end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
