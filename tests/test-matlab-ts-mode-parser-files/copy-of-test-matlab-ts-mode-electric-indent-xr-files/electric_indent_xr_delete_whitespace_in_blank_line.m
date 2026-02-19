% -*- matlab-ts -*-

% Validate that if a line contains whitespace and we "RET" on it, that we delete the spaces

% Case1: (t-utils-xr (re-search-forward "bar") "C-e" "C-m" "C-m" (insert "x=1 ;") "C-i")

if foo == 1
    bar = 1;
end

% Case2: (t-utils-xr (re-search-backward "^if") (t-utils-xr-print-code (point) (re-search-forward "end\n")))
