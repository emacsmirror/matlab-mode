% -*- matlab-ts -*-

% See https://github.com/acristoffers/tree-sitter-matlab/issues/118

% t-utils-test-indent: no-line-by-line-indent - the "...           % comment1" isn't handled typing line-by-line

c1 = {'foo'...               
      ...           % comment1
      'bar'...      % comment2
     };
