% -*- matlab-ts -*-

% Following are non-numeric m-matrices:

a = 2;
b = 3;

m1 = [1, a + b
         3,     4];

% Following are not m-matrices:
notM1 = [1, 2; a+b;
         5, 6; 7 6];

notM2 = [1, 2, ...
         3 a+b;
         5, 6, ...
         7 6];
