% -*- matlab-ts -*-

%{
  (t-utils-xr

  (re-search-forward "^%}") "C-n"
  (insert "function out=indent_xr_fun3(in1, ...")   "C-m"
  (insert                             "in2)")       "C-m"
  (insert      "out = in1 + in2;")                  "C-m"
  (insert "end")
  (re-search-backward "^fun")
  (t-utils-xr-print-code (point) (point-max))
  (delete-region (point) (point-max))

  )
%}
