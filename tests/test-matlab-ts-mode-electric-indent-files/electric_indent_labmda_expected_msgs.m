% -*- matlab-ts -*- %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

% t-utils-test-indent: no-line-by-line-indent - when we type line-by-line, the continuation lines %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
% have a syntax error because the continued portion isn't there. %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher matlab-ts-mode--i-block-comment-end-anchor 0)}>


l1 = @(x) ((ischar(x) || isstring(x))); %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

l2 = @(x) ((ischar(x) || isstring(x) || isnumeric(x)) && ... %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
           ~strcmpi(x, 'fubar')); %  <{Matched rule: ((parent-is "\\`\\(?:\\(?:boolea\\|compariso\\)n_operator\\)\\'") parent 0)}>

l3 = @(x) disp(x); %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
