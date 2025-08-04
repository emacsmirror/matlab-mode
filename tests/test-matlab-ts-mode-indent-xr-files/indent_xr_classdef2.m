% -*- matlab-ts -*-

%{

  (t-utils-xr
  (re-search-forward "%}")                  "C-n"
  (insert "classdef indent_xr_classdef2")   "C-m"
  (insert     "properties")                 "C-m"
  (insert          "p1")                    "C-m"
  (insert          "p2 double;")            "C-m"
  (insert          "p3continued = ...")     "C-m"
  (insert              "0")                 "C-m"
  (insert     "end")                        "C-m"
  (insert "end")
  (re-search-backward "^classdef")
  (print (buffer-substring-no-properties (point) (point-max)))
  (delete-region (point) (point-max))
  )
%}
