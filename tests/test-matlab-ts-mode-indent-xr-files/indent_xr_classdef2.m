% -*- matlab-ts -*-

%{

  (t-utils-xr
  (re-search-forward "^classdef") "C-e" "C-m"
  (insert "properties") "C-m"
  (insert "p1") "C-m"
  (insert "p2 double;") "C-m"
  (insert "end") "C-m"
  (insert "end")
  (re-search-backward "^classdef")
  (print (buffer-substring-no-properties (point) (point-max)))
  )
%}
classdef indent_xr_classdef2
