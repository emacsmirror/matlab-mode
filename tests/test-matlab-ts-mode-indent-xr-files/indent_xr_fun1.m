% -*- matlab-ts -*-
% Case1: (t-utils-xr "C-a" "C-n" "C-e" "C-m" (insert "a = 1;") "C-m" (insert "end") "C-m" "C-p" "C-p" "C-i"  (re-search-backward "^fun") (print (buffer-substring-no-properties (point) (point-max))))
function indent_xr_fun1
