% -*- matlab-ts -*- %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

% t-utils-test-indent: no-line-by-line-indent - when typing line-by-line we can't align the %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
% trailing comments because we don't see later lines %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher matlab-ts-mode--i-block-comment-end-anchor 0)}>

x   = [1, 2, 3]; % comment 1 (aligned) %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
xyz = 2;         % comment 2 (aligned) %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>


if x %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
    y = 1 %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
end % end if x %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
% other comment %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher matlab-ts-mode--i-block-comment-end-anchor 0)}>
z = y; %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

if x %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
    y = 1 %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
end % end if x %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
x = 1 % this shouldn't be aligned with prior %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

if z % some comment %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
    y = [1, 2] % some comment %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>

foo1 blah % comment (aligned) %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
c = 1;    % comment (aligned) %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

;     % comment (aligned) %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
cfooo % comment (aligned) %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

if a && ... %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
   b % comment %  <{Matched rule: ((parent-is "\\`\\(?:\\(?:boolea\\|compariso\\)n_operator\\)\\'") parent 0)}>
    z = [1, 2, 2, 4] % comment (not aligned) %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
