% -*- matlab-ts -*-
function result = indent_xr_continuation2(a, b)
% (t-utils-xr "C-e" "C-m" (insert "result = 1 + myfcn(a, b) + ...") "C-m" (insert "b);") (re-search-backward "^[ ]*result") (print (buffer-substring-no-properties (point) (point-max))))
end
