% -*- matlab-ts -*-

% Case1: (t-utils-xr (re-search-forward "first") "C-M-b" "C-M-f")
a = "first string %d \t ''"

% Case2: (t-utils-xr (re-search-forward "second") "C-M-b" "C-M-f")
b = 'second asdf" %d \t \n';

% Case3: (t-utils-xr (re-search-forward "%d") "C-b" "C-M-b" "C-M-f")
c = 'third asdf" %d \t \n';
