% -*- matlab-ts -*-
% (t-utils-xr "C-a" "C-n" (insert "cell1 = { ...") "C-e" "C-m" (insert "{'one', ...") "C-m" (insert "'two'}, ...") "C-m" (insert "...") "C-m" (insert "{'three'}") "C-m" (insert "};") "C-m" (re-search-backward "^cell") (print (buffer-substring-no-properties (point) (point-max))))
