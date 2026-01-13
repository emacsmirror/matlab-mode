% -*- matlab-ts -*- %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

% t-utils-test-indent: no-line-by-line-indent - need all lines to align trailing comments %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

x   = 1;            % comment %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
foo = [1, 2, 3, 4]; % comment %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
b   = 3;            % comment %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

% separator comment %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

a(1, 2) = [1, 2, 3]; % comment %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
b(1, 2) = 1;         % commnet %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

% We don't align the trailing comments in the following becaue of the "%" in the string %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
% matlab-ts-mode could be updated to handle this case, but it's likely rare. %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher matlab-ts-mode--i-block-comment-end-anchor 0)}>

foo       = 1; % comment 1 %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
d         = dictionary("%", 1); % comment 2 %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
a(d("%")) = [1, 2, 3]; % comment 3 %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
