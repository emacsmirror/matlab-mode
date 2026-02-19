% -*- matlab-ts -*-

% Exercise TAB indent which will indent consecutive lines

% Case1: (t-utils-xr (re-search-forward "a1") "C-i")

foo = 1;
a1       =2;
longvar        = 3  ;

% Case2: (t-utils-xr (re-search-forward "foo") "C-i")

m1 = [1,2 1 -1 -2 -3 -4 - 5 - 6
      foo + a1 1 2 3 4 5 6];
