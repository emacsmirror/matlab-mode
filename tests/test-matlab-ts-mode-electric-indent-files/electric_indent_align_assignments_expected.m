% -*- matlab-ts -*-

% Consecuitvie assignment alignment

% t-utils-test-indent: no-line-by-line-indent - when we type line-by-line, we don't see later
%                      assignment values, thus to fully indent, need to re-indent the file after
%                      typing line-by-line.

a             = 10;
bLongVariable = [1, 2, 3];
c             = 5;
dShort        = [1, 2];
x             = a + bLongVariable + c + dShort;
disp(x);

length = 5;
width  = 10;
area   = length * width;

% other

a          = 1.234;
abc        = [1, 2, 5];
abcdefg    = [1, 2; 3, 44];
x          = 1;
f(a(x), 1) = 10;
g          = foo(1, 2, 3);

% some more

x = 2;
y = 3;
z = x * y;
