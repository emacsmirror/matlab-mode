% -*- matlab-ts -*- %  <{Matched rule: (maltab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
% function args have syntax error %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher parent 0)}>
function foo=indent_with_syntax_error(bar1, ... % a comment for bar1 %  <{Matched rule: (maltab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
                                      % help comment %  <{Matched rule: (matlab-ts-mode--i-next-line-matcher matlab-ts-mode--i-next-line-anchor matlab-ts-mode--i-next-line-offset)}>
                                      foo = bar1 + bar2 * 2; %  <{Matched rule: (matlab-ts-mode--i-next-line-matcher matlab-ts-mode--i-next-line-anchor matlab-ts-mode--i-next-line-offset)}>
end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
