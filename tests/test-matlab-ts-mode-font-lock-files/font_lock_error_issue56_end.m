% -*- matlab-ts -*-

% See: https://github.com/acristoffers/tree-sitter-matlab/issues/56


v = 1:10
w = v(2:(end-3))
x = v(2:abs((end-3)*2)/2)

z = v([2 end]);
