% -*- matlab-ts -*-

% Following are m-matrices:

m1 = [1, 2, 3
     4, 5 6];

m2 = [1, 2, 3;
     4, 5 6];

m3 = [1, 2, 3
     4, 5 6];

m4 = [1, 2, 3; ...
     4, 5 6];

m5 = [1, 2, 3; ...
       
      4, 5, 6];

m6 = [1, 2, 3; ...
       % comment
      4, 5, 6];

m7 = [1, 2, 3; ...

       % comment
      4, 5, 6];

m8 = [
       1, 2, 3 % comment 1
       4, 5, 6 % comment 2
       7, 8, 9 % comment 3
     ];

% Following are not m-matrices:
notM1 = [1, 2; 3 4;
         5, 6; 7 6];

notM2 = [1, 2, ...
         3 4;
         5, 6, ...
         7 6];
