% -*- matlab-ts -*-

%{
  Case1:
  (t-utils-xr

  (re-search-forward "%}") "C-n" "C-m" "C-b"

  (insert "var_a = my_function(1, ...")          "C-m"
  (insert                     "2, ...")          "C-m"
  (insert                     "3);")             "C-m"

  (insert "var_b = my_function( ...")            "C-m"
  (insert     "1, ...")                          "C-m"
  (insert     "2, ...")                          "C-m"
  (insert     "3);")                             "C-m"

  (re-search-backward "^var_a")

  (t-utils-xr-print-code (point) (point-max))

  )
%}
