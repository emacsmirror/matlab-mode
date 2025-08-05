% -*- matlab-ts -*-

%{
  (t-utils-xr

  (re-search-forward "%}")  "C-n"
  (insert "someVariable = {")      "C-m"
  (insert "1234")                  "C-m"
  (insert "};")                    "C-m"
  (re-search-backward "^someVariable")
  (t-utils-xr-print-code (point) (point-max))

  )
%}
