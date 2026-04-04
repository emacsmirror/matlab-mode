% -*- matlab-ts -*-

% t-utils-test-indent: no-line-by-line-indent - we need all lines to align

fooBarGoo = [0; 0; % comment 1
             3; 4;  % comment 2
             8; 0]; % Comment 3

xvals = cast([ % comment 0
                      1.51266667212 % comment 1
               2.236550782529778999 % comment 2
                  1.882832775783885 % comment 3
                     6.036100699764 % comment 4
                     1.239375584911 % comment 5
               1.998030698880604271 % comment 6
                      2.78762992099], ...  % comment 7
             class(eg));
