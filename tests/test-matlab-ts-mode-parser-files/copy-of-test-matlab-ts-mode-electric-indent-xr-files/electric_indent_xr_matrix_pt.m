% -*- matlab-ts -*-

% case1: (t-utils-xr (re-search-forward "223") (backward-char 3) "C-i")

m3a = [    21    ,          2, 223;
          4, 53333,   6; ...
       1222,     4,   5];

% case2: (t-utils-xr (re-search-forward "444") "C-a" "C-i")

m3b = [    21    ,          2, 223;
            444, 53333, 6; ...
       1222,     4,   5];
