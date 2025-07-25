% -*- matlab-ts -*-
% (t-utils-xr "C-n" "C-e" "C-m" (insert "result = 1 + myfcn(a, ...") "C-m" (insert "b);") "C-m" (re-search-backward "^fun") (print (buffer-substring-no-properties (point) (point-max))))
function result = indent_xr_i_cont_incomplete1(a, b)
end
