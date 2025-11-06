% -*- matlab-ts -*-

% See https://github.com/acristoffers/tree-sitter-matlab/issues/60

person = struct('Age', {30, 25, 15}, 'City', {'c1', 'c2', 'c3'});
[person([1:3,1]).names] = deal('no-name');
