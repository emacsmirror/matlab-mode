% -*- matlab-ts -*-

%{
  (t-utils-xr
  (re-search-forward "%}") "C-n"

  (insert "classdef electric_ends_classdef")    "C-m"
  (insert     "enumeration")                    "C-m"
  (insert          "red")

  (re-search-backward "^classdef")
  (t-utils-xr-print-code (point) (point-max))
  )
%}
