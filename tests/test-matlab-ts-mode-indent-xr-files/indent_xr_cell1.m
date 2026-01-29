% -*- matlab-ts -*-

%{
  Case1:
  (t-utils-xr

  (re-search-forward "%}")  "C-n"

  ;; ensure final new line for tree-sitter
  (insert "\n")                    "C-b"

  (insert "someVariable = {")      "C-m"
  (insert "1234")                  "C-m"
  (insert "};")                    "C-m"
  (re-search-backward "^someVariable")
  (t-utils-xr-print-code (point) (point-max))

  )
%}
