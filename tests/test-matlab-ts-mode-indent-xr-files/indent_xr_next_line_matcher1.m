% -*- mode: matlab-ts; matlab-ts-mode-electric-ends: nil -*-
% Case1: (t-utils-xr "C-n" "C-e" "C-m" (insert "properties") "C-m" (insert "end") "C-i" "C-e" "C-m" (insert "end") "C-m" (re-search-backward "^classdef") (print (buffer-substring-no-properties (point) (point-max))))
classdef indent_xr_next_line_matcher1
