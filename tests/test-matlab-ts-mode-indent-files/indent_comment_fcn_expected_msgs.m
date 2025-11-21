% -*- matlab-ts -*- %  <{Matched rule: (maltab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
% %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher parent 0)}>
% t-utils-test-indent: no-line-by-line-indent - nested functions require an end %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher parent 0)}>

function indent_comment_fcn %  <{Matched rule: (maltab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
% This is the %  <{Matched rule: (matlab-ts-mode--i-doc-comment-matcher matlab-ts-mode--i-doc-comment-anchor matlab-ts-mode--i-doc-comment-offset)}>
% doc help comment %  <{Matched rule: (matlab-ts-mode--i-doc-comment-matcher matlab-ts-mode--i-doc-comment-anchor matlab-ts-mode--i-doc-comment-offset)}>

    nested(); %  <{Matched rule: ((parent-is "\\`function_definition\\'") parent matlab-ts-mode--set-function-indent-level-for-gp)}>

    function nested %  <{Matched rule: ((n-p-gp "\\`function_definition\\'" "\\`block\\'" "\\`function_definition\\'") parent 0)}>

        % this is a nested comment for code %  <{Matched rule: ((parent-is "\\`function_definition\\'") parent matlab-ts-mode--set-function-indent-level-for-gp)}>
        disp('in nested'); %  <{Matched rule: ((parent-is "\\`function_definition\\'") parent matlab-ts-mode--set-function-indent-level-for-gp)}>
        nested2; %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
    end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>

    function nested2 %  <{Matched rule: ((n-p-gp "\\`function_definition\\'" "\\`block\\'" "\\`function_definition\\'") parent 0)}>
    % comment for nested2 %  <{Matched rule: (matlab-ts-mode--i-doc-comment-matcher matlab-ts-mode--i-doc-comment-anchor matlab-ts-mode--i-doc-comment-offset)}>
        disp('in nested2'); %  <{Matched rule: ((parent-is "\\`function_definition\\'") parent matlab-ts-mode--set-function-indent-level-for-gp)}>
        % comment after code %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
    end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
