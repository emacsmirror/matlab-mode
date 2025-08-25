% -*- mode: matlab-ts; matlab-ts-mode-electric-ends: nil -*-
%{
  Case1:
  (t-utils-xr

  (re-search-forward "^switch")  "C-e" "C-m"
  (insert "case 1")                    "C-m"
  (insert "disp('1');")                "C-m"
  (insert "end")                       "C-m"

  (re-search-backward "^switch")
  (t-utils-xr-print-code (point) (point-max))

  )
%}
switch fcn1(a)
