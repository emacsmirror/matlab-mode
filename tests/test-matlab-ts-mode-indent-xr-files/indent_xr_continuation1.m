% -*- matlab-ts -*-
function result = indent_xr_continuation1(a, b)
% (t-utils-xr "C-e" "C-m" (insert "result = 1 + myfcn(a, ...") "C-m" (insert "b);") "C-m" (re-search-backward "^[ ]*result") (print (buffer-substring-no-properties (point) (point-max))))
end
