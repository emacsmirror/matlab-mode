% -*- matlab-ts -*-

% Exercise matlab-ts-mode--ei-align-line-in-m-matrix

% t-utils-test-indent: no-line-by-line-indent - when typing matrix line-by-line, there are
% error nodes and thus the matrix alignment doesn't occur

m = [1111,     2, 223
      ...
        4, 53333,   6];

                           
m1 = [           ...
         1 2222; ...
                 ...
       333    4; ...
     ];

m1b = [
          1 2222 % comment 1
        333    4 % comment 2
      ];

m2 = [ ...
         1 2222
       333    4
     ];

m3a = [  21,     2, 223;
          4, 53333,   6; ...
       1222,     4,   5];

m3b = [   1111,        2 ,223;
             4, 53333,   6; ...
         1222,     4,   5;];

m3c = [1111,   2, 223;
       4, 533,   6; ...
       1222,   4,   5;];

a = 1;
b = 2;
c = 3;
d = 4;

m4a = [a * b + c, d + a;
               b,     c];

m4b = [a * b + c, d + a;
               b,     c];

m5a = [a * b + c d + a; ...
        ...
        ...
               b     c];

m6 = [ ...
       ...
        1 2222
       ...
       ... comment
      333    4
     ];

m7 = [   1111; ...
           22; ...
          333; ...
      1111111];



not1 = [1111, ...
        22; ...
        333, ...
        1111111];

not2 = [1     2 223;
          [4 53333   6]; ...
        1222     4   5; ...
     ];

not3 = [ ...
       1 2 3; 4 5 6; ...
       1 4 5; ...
     ];

not4 = [a * b + c, [d + a];
        b, c];

not5 = [ ...
       1 2222; ...
       333 4; ...
       ]; if 1, disp('here')
          end

not6 = [1 2;
        3 4 5];
