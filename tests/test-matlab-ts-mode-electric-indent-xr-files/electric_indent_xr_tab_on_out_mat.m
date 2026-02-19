% -*- matlab-ts -*-

% Case1: (t-utils-xr (re-search-forward "1 ,") "C-i")
if x
a([  1 ,   2], :) = ...                                          
    [ 1,2   3; ...
  100 200,300];
end

% Case2: (t-utils-xr (re-search-backward "^if") (t-utils-xr-print-code (point) (re-search-forward "end\n")))
