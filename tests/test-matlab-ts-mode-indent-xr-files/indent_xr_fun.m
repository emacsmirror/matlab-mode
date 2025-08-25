% -*- matlab-ts -*-
%{
  Case1:
  (t-utils-xr
  (re-search-forward "%}") "C-n"
  (insert "function a = indent_xr_fun()")   "C-m"
  (insert     "a = 1;")                     "C-m"
  (insert "end")
  (re-search-backward "^fun")
  (t-utils-xr-print-code (point) (point-max))
  )
%}
