% -*- matlab-ts -*-

% See: https://github.com/acristoffers/tree-sitter-matlab/issues/67

a=1:10;
[a(sum([1 1]))] = -99;
[a([a(4)])] = -99;


