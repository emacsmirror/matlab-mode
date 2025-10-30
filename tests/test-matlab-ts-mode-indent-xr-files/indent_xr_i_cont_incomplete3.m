% -*- matlab-ts -*-
%{
  Case1:
  (t-utils-xr

  (re-search-forward "^fun") "C-e" "C-m"
  (insert "output1 = ...")         "C-m"
  (insert "99;")                   "C-i"

  (re-search-backward "^fun")
  (t-utils-xr-print-code (point) (point-max))

  )
%}
function output1 = indent_xr_i_cont_incomplete3
end
