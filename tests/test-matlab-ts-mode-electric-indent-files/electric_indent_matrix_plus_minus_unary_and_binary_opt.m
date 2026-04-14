% -*- matlab-ts -*-

% t-utils-test-indent: no-line-by-line-indent - when typing matrix line-by-line, there are
% error nodes and thus the matrix alignment doesn't occur

mNonNumeric1 = [
       1 a+b
       3 a-b
     ];

mNonNumeric2 = [
       1 a+ b
       3 a- b
     ];

mNonNumeric3 = [
       1 a + b
       3 a - b
     ];
