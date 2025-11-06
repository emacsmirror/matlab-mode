% -*- matlab-ts -*-

A = [1+2i, 3; 4, 5-6i]; % A complex matrix

B = A';   % Complex conjugate transpose
C = A.';  % Non-conjugate transpose

disp('Complex conjugate transpose (A''):');
disp(B);

disp('Non-conjugate transpose (A.''):');
disp(C);

