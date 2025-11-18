% -*- matlab-ts -*- %  <{Matched rule: (maltab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
function indent_copyright %  <{Matched rule: (maltab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
% indent_copyright help %  <{Matched rule: (matlab-ts-mode--i-doc-comment-matcher matlab-ts-mode--i-doc-comment-anchor matlab-ts-mode--i-doc-comment-offset)}>
% comment %  <{Matched rule: (matlab-ts-mode--i-doc-comment-matcher matlab-ts-mode--i-doc-comment-anchor matlab-ts-mode--i-doc-comment-offset)}>
% that is several %  <{Matched rule: (matlab-ts-mode--i-doc-comment-matcher matlab-ts-mode--i-doc-comment-anchor matlab-ts-mode--i-doc-comment-offset)}>
% lines long %  <{Matched rule: (matlab-ts-mode--i-doc-comment-matcher matlab-ts-mode--i-doc-comment-anchor matlab-ts-mode--i-doc-comment-offset)}>

% Copyright after help comment shouldn't be indented %  <{Matched rule: (matlab-ts-mode--i-doc-comment-matcher matlab-ts-mode--i-doc-comment-anchor matlab-ts-mode--i-doc-comment-offset)}>

    % this is a comment about the following code %  <{Matched rule: ((parent-is "\\`function_definition\\'") parent matlab-ts-mode--set-function-indent-level-for-gp)}>
    disp('here') %  <{Matched rule: ((parent-is "\\`function_definition\\'") parent matlab-ts-mode--set-function-indent-level-for-gp)}>

end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
