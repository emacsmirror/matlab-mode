% -*- matlab-ts -*-
%{
  (t-utils-xr
  (re-search-forward "^fun") "C-e"                "C-m"
  (insert     "result = longFunction( ...")       "C-m"
  (insert          "99);")
  (re-search-backward "^fun")
  (t-utils-xr-print-code (point) (point-max))
  )
%}
function result = indent_xr_i_cont_incomplete5
end
