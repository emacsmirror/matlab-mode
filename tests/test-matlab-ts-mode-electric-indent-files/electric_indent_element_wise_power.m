% -*- matlab-ts -*-

% .^ operator is element-wise power

% MATLAB tree-sitter identifies 10. as a number, thus 10.^5 and 10 .^ 5 have differnt node types.

a = 5;
b = 10;

x = 10 .^ 5
y = 10 .^ a;
z = a.^b;
