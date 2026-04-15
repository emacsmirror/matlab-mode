% -*- matlab-ts -*-

% t-utils-test-indent: no-line-by-line-indent - when typing matrix line-by-line, there are
% error nodes and thus the matrix alignment doesn't occur

m5([1, 2; 3, 4]) = [     101,       21
                     % comment1
                           1,        2; ... extra text
                        3001,     4001,
                    -1.0e-10, +1.2e+10 % comment2
                           a,       -b],,; % comment3

m6([1, 2; 3, 4]) = [     101,       21
                     % comment1
                           1,        2; ... extra text
                        3001,     4001,
                    -1.0e-10, +1.2e+10 % comment2
                           a,       -b],,; ... comment3

m7([1, 2; 3, 4]) = [     101,       21
                     % comment1
                           1,        2; ... extra text
                        3001,     4001,
                    -1.0e-10, +1.2e+10 % comment2
                           a,       -b]; % comment

m8([1, 2; 3, 4]) = [     101,       21
                     % comment1
                           1,        2; ... extra text
                        3001,     4001,
                    -1.0e-10, +1.2e+10 % comment2
                           a,       -b];

m9([1, 2; 3, 4]) = [     101,       21
                     % comment1
                           1,        2; ... extra text
                        3001,     4001,
                    -1.0e-10, +1.2e+10 % comment2
                           a,       -b]
