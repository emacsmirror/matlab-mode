% -*- matlab-ts -*-

% Case1: (t-utils-xr (re-search-forward (rx "a+")) "C-a" "C-i")

n1 = [
          1, a+b
          % comment
           c-d,     1
              5   600
         ];

% Case2: (t-utils-xr (re-search-forward (rx "a+")) "C-i")

n2 = [
          1, a+b
          % comment
           c-d,     1
              5   600
         ];
