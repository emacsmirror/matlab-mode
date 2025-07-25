% -*- matlab-ts -*-
% (t-utils-xr "C-n" "C-e" "C-m" (insert "result = 1 + myfcn(a, b) + ...") "C-m" (insert "b);") (re-search-backward "^fun") (print (buffer-substring-no-properties (point) (point-max))))
function result = indent_xr_i_cont_incomplete2(a, b)
end
