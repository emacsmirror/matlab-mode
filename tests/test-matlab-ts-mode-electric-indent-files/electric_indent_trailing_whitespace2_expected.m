% -*- matlab-ts -*-

% t-utils-test-indent: no-line-by-line-indent - when we type line-by-line, we don't see later error nodes

% lines with trailing whitespace (electric indent removes it)

foobar1234 = [3, 5, 10, 30, 35, 40, 45, 50, 60, 70, 80, 90, 100]; % comment 1
other1 = [ 15,  25, 133, 160, 188, 216, 242, 270, NaN, NaN; % comment 2
          NaN,  11,  65,  78,  92, 106, 119, 133, 162, 189; % comment 3
          NaN, NaN,  31,  38,  44,  51,  58,  65,  79,  93]; % comment 4
