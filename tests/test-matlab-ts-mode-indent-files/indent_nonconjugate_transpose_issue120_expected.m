% -*- matlab-ts -*-

% See https://github.com/acristoffers/tree-sitter-matlab/issues/120

a = [1 2; 3 4];
b = a;

m1 = [a.'; b.'];

c1 = {a.'; b.'};
