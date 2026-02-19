% -*- matlab-ts -*-

% Exercise RET indent which will indent consecutive lines

% Case1: (t-utils-xr (re-search-forward "a1") "C-m")

foo = 1;
a1       = 2;
longvar        = 3  ;

% Case2: (t-utils-xr (re-search-forward "3") "C-e" "C-m")

foo = 1;
a1       = 2;
longvar        = 3  ;

% Case3: (t-utils-xr (re-search-forward "6") "C-m")

m1 = [1,2 1 -1 -2 -3 -4 - 5 - 6
      foo + a1 1 2 3 4 5 6];

% Case4: (t-utils-xr (re-search-forward "3;") (forward-line) (insert "x=1;") "C-m")

a1      = 1;
foo1    = 2;
foobar1 = 3;

% Case5: (t-utils-xr (t-utils-xr-print-code (point-min) (point-max)))
