% -*- matlab-ts -*-

% Case1: (t-utils-xr "C-n" "C-e" "C-i")
if x
       a - b *2;
end

% Case2: (t-utils-xr (re-search-backward "^if") (t-utils-xr-print-code (point) (re-search-forward "end\n")))

