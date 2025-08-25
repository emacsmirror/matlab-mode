% -*- matlab-ts -*-
%{
  Case1:
  (t-utils-xr
  (re-search-forward "^fun") "C-e"            "C-m"
  (insert     "result = 1 + myfcn(a, ...")    "C-m"
  (insert                        "b);")
  (re-search-backward "^fun")
  (t-utils-xr-print-code (point) (point-max))
  )
%}
function result = indent_xr_i_cont_incomplete1(a, b)
end
