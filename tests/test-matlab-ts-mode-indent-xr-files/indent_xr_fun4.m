% -*- mode: matlab-ts; matlab-ts-mode-electric-ends: nil -*-

%{
  Case1:
  (t-utils-xr

  (re-search-forward "%}") "C-n"

  (insert "\n") "C-b"  ;; ensure we have a newline

  (insert "function a=indent_xr_fun4")    "C-m"
  (insert "a=1;")                         "C-m"
  (insert "%comment")                     "C-m"
  (insert "end")                          "C-m"

  (re-search-backward "^fun")
  (t-utils-xr-print-code (point) (point-max))
  )
%}
