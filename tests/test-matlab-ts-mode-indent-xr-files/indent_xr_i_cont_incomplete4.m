% -*- matlab-ts -*-
% Case1: (t-utils-xr "C-n" "C-e" "C-m" (insert "a = foo((1+2)*3, a, ...") "C-m" (insert "99);") (re-search-backward "^fun") (print (buffer-substring-no-properties (point) (point-max))))
function a = indent_xr_i_cont_incomplete4
end
