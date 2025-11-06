% -*- matlab-ts -*-

s1 = "abc";
s2 = 'One\nTwo';
s3 = sprintf("see %d:%d", 1, 2)

A = [1 2; 3 4];
B = A';   % transpose
C = A'';  % transpose transpose ==> isequal(C,A)
D = A'''; % transpose transpose transpose ==> isequal(D,B)
