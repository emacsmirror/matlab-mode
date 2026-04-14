% -*- matlab-ts -*-

% Non-numeric matrices: binary +/- operators make these non-numeric.

mNonNumeric1 = [
       1 a+b
       3 a-b
     ];

mNonNumeric2 = [
       1 a+ b
       3 a- b
     ];

mNonNumeric3 = [
       1 a + b
       3 a - b
     ];

% Numeric matrices: unary +/- are valid numeric entries.

mNumeric1 = [
       1 a +b
       2 2,+2
       3 a,-b
       4.1 4.1 -4.3
     ];

% Numeric matrices with exponents
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

% Unary minus at start of row
mUnaryStart = [
       -1  2
        3 +4
      ];

% Unary with tilde
mUnaryTilde = [
       ~a  2
        3  4
      ];

% Single column with unary
mSingleCol = [
       -1
       +2
        3
      ];
