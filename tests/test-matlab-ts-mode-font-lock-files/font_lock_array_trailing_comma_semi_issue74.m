% -*- matlab -*-

% https://stackoverflow.com/questions/32849720/why-is-a-trailing-comma-in-a-cell-array-valid-matlab-syntax
% See: https://github.com/acristoffers/tree-sitter-matlab/issues/74

c1 = {'foo',}

c2 = {{'foo',};;; {'bar',};;;}

m1 = [1,]

m2 = [1,2,3;;; 4,5,6;;;;]
