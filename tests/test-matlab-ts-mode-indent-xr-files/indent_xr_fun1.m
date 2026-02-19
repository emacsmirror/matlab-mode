% -*- mode: matlab-ts; matlab-ts-mode-electric-ends: nil -*-

%{
  Case1:
  (t-utils-xr

  (re-search-forward "%}") "C-n"

  "C-m" "C-b"

  (insert "function indent_xr_fun1")     "C-m"

  (insert "a = 1;")                      "C-m"

  (insert "end") "C-m"

  (re-search-backward "^fun")
  (print (buffer-substring-no-properties (point) (point-max)))

  )
%}
