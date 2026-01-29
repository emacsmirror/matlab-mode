% -*- matlab-ts -*-

% t-utils-test-indent: no-line-by-line-indent - when we type line-by-line, the continuation lines
% have a syntax error because the continued portion isn't there.


l1 = @(x) ((ischar(x) || isstring(x)));

l2 = @(x) ((ischar(x) || isstring(x) || isnumeric(x)) && ...
           ~strcmpi(x, 'fubar'));

l3 = @(x) disp(x);
