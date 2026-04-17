% -*- matlab-ts -*- %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

% t-utils-test-indent: no-line-by-line-indent - when we type line-by-line, we don't see later error nodes %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

% lines with trailing whitespace (electric indent removes it) %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

foobar1234 = [3, 5, 10, 30, 35, 40, 45, 50, 60, 70, 80, 90, 100]; % comment 1 %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
other1 = [ 15,  25, 133, 160, 188, 216, 242, 270, NaN, NaN; % comment 2 %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
          NaN,  11,  65,  78,  92, 106, 119, 133, 162, 189; % comment 3 %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
          NaN, NaN,  31,  38,  44,  51,  58,  65,  79,  93]; % comment 4 %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
