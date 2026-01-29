% -*- matlab-ts -*- %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

% t-utils-test-indent: no-line-by-line-indent - line-by-line typing results in error nodes %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

function electric_indent_start_pt_offset() %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

% This exercises %  <{Matched rule: ((parent-is "\\`function_definition\\'") parent matlab-ts-mode--set-function-indent-level-for-gp)}>
%     (when (> start-pt-offset eol-col) ;; be robust too big a start-pt-offset %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher matlab-ts-mode--i-block-comment-end-anchor 0)}>
%       (setq start-pt-offset eol-col))) %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher matlab-ts-mode--i-block-comment-end-anchor 0)}>
% in matlab-ts-mode--ei-move-to-loc %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher matlab-ts-mode--i-block-comment-end-anchor 0)}>

foobar('Parameters', ... %  <{Matched rule: ((parent-is "\\`function_definition\\'") parent matlab-ts-mode--set-function-indent-level-for-gp)}>
       {'A', 'B', 'C', 'D', 'E', 'F'}, ... %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
       'Values', ... %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
       {1, 1, 1, 1, 1, 1}); %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
