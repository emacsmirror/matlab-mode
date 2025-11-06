% -*- matlab-ts -*-

% See https://github.com/acristoffers/tree-sitter-matlab/issues/53

v  = 1:10
g1 = v(2:end)
g2 = v(end:end)
g3 = v(end-2)
g4 = v(end+2-3)

a = 1:10

b = a(end)
c = a(end, :)
d = a(end, end)

x = [1, 2
     3, 4];
y = [5, 6
     7, 8];
z = [9, 10
     11, 12];

xyz = cat(3, x, y, z)

x1 = xyz(end, :, :)
x2 = xyz(end, end, :)
x3 = xyz(:, :, end)
x4 = xyz(end, end, end)
