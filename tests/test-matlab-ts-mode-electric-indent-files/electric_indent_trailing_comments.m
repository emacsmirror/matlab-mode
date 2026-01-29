% -*- matlab-ts -*-

% t-utils-test-indent: no-line-by-line-indent - need all lines to align trailing comments

   x = 1; % comment
  foo = [1, 2, 3, 4]; % comment
         b = 3; % comment

% separator comment

  a(1,2) = [1, 2, 3]; % comment
   b(1,2) = 1; % commnet

% We don't align the trailing comments in the following becaue of the "%" in the string
% matlab-ts-mode could be updated to handle this case, but it's likely rare.

  foo = 1; % comment 1
   d = dictionary("%", 1); % comment 2
a(d("%")) = [1, 2, 3]; % comment 3
