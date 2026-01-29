% -*- matlab-ts -*-

% t-utils-test-indent: no-line-by-line-indent - when typing matrix line-by-line, there are
% error nodes and thus the matrix alignment doesn't occur

m6 = [1 2
      3 4];

m7 = [   1111   22
      1111111, 2222];

m8 = [
          1111; ...
       1111111
     ];

m = [   2, 4000
     3000,    1]
