% -*- matlab-ts -*-

% See; https://github.com/acristoffers/tree-sitter-matlab/issues/71

y = 7;
fh = @(x)x.^2+y;
s = functions(fh)
s.function

