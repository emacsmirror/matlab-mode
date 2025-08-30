% -*- matlab-ts -*-

% See https://github.com/acristoffers/tree-sitter-matlab/issues/107

v1 = {1 + 2};

v2 = { ...
       1 ...
       + ...
       2
     };

assert(isequal(v1, v2))

v3 = [1 + 2];

v4 = [ ...
       1 ...
       + ...
       2
     ];

assert(isequal(v3, v4))

v5 = 1 + 2;

v6 = (1 ...
      + ...
      2);

assert(isequal(v5, v6))
