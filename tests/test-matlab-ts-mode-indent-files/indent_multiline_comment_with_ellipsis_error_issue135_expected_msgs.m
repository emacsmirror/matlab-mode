% -*- matlab-ts -*- %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

% See: https://github.com/acristoffers/tree-sitter-matlab/issues/135 %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

% t-utils-test-indent: no-line-by-line-indent - multi-line comment below can not be indented %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
% line-by-line %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher matlab-ts-mode--i-block-comment-end-anchor 0)}>


a = 1; %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
b = 2; %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

if a == 10 || ... %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
   %{ %  <{Matched rule: ((parent-is "\\`\\(?:\\(?:boolea\\|compariso\\)n_operator\\)\\'") parent 0)}>
     block %  <{Matched rule: (matlab-ts-mode--i-in-block-comment-matcher parent 2)}>
     comment %  <{Matched rule: (matlab-ts-mode--i-in-block-comment-matcher parent 2)}>
   %} %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher matlab-ts-mode--i-block-comment-end-anchor 0)}>
   b == 2 %  <{Matched rule: ((parent-is "\\`\\(?:\\(?:boolea\\|compariso\\)n_operator\\)\\'") parent 0)}>
    disp('here') %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
