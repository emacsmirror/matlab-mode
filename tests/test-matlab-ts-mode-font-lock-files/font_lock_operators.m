% -*- matlab-ts -*-
aa = inf;

x = 1 + 2 - 3 * 4 / 5

a = [1 2 3; 4 5 6];
b = [2 2 2; 1 1 1];
c = a .* b

d = a .\ b

e = a .^ 2

f = [1 2; 3 4] ^ 2

g = a.'
h = a'

j = 1 == 2

k = 1 ~= 2

l = 3 > 3
m = 3 >= 3
n = 3 < 3
o = 3 <= 3

b1 = true & false;
b2 = true | false;
b3 = true && false;
b4 = true || false;
b5 = ~b4;

sqr = @(x) x .^ 2;

mc = ?MyClass;

