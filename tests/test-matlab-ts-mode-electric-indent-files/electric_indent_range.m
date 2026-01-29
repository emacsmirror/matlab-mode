% -*- matlab-ts -*-

% t-utils-test-indent: no-line-by-line-indent - when we type line-by-line, the continuation lines
% have a syntax error because the continued portion isn't there.

dataTbl = [dataTbl(:, 1:varColNumbers(1)-1)...
           tableWithDateColumnOnly dataTbl(:, varColNumbers(1) + 1 : end)];
