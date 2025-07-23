% -*- matlab-ts -*-
% (t-utils-xr "C-a" "C-n" (insert "function a = indent_xr_fun()") "C-m" (insert "a = 1;") "C-m" (insert "end") "C-m" (re-search-backward "^fun") (print (buffer-substring-no-properties (point) (point-max))))
