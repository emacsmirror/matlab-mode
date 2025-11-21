% -*- mode: matlab-ts; matlab-ts-mode-electric-ends: nil -*-

%{

  Case1:
  (t-utils-xr
  (re-search-forward "%}")                  "C-n"
  (insert "classdef indent_xr_classdef2")   "C-m"
  (insert     "properties")                 "C-m"
  (insert          "p1")                    "C-m"
  (insert          "p2 double;")            "C-m"
  (insert          "p3continued = ...")     "C-m"
  (insert              "0")                 "C-m"
  (insert     "end")                        "C-m"
  (insert "end")                            "C-m"
  (re-search-backward "^classdef")
  (t-utils-xr-print-code (point) (point-max))
  )
%}
