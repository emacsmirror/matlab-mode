% -*- matlab-ts -*-

% t-utils-test-indent: no-line-by-line-indent - when typing matrix line-by-line, there are
% error nodes and thus the matrix alignment doesn't occur

mNumeric = [
       1 a +b
       2 2,+2
       3 a,-b
       4.1 4.1 -4.3
     ];

mNumeric2 = [1   2e-3
              2 2.1e-3
              3   2E-3
              4 2.1E-3
              5   2d-3
              6 2.1d-3
              7   2D-3
              8 2.1D-3

              1, -2e-3
              2,-2.1e-3
              3,-2E-3
              4,-2.1E-3
              54321,-2d-3
              6,-2.1d-3
              7,-2D-3
              8,-2.1D-3

              1  +2e-3
              2 +2.1e-3
              3   +2E-3
              4 +2.1E-3
              5   +2d-3
              6 +2.1d-3
              7   +2D-3
              8 +2.123D-3];
