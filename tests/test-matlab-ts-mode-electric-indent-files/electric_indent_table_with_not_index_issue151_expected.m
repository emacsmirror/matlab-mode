% -*- matlab-ts -*-

% See https://github.com/acristoffers/tree-sitter-matlab/issues/151

T = table([1; 2], [5; 10]);
a = 1;
T.(~a + 1)
T.(1 + ~a)
