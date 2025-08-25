% -*- mode: matlab-ts; matlab-ts-mode-electric-ends: nil -*-

%{
  - First row of commands: we validate multiple indents don't cause point movement
  - Remaining rows add come class content

  Case1:
  (t-utils-xr
  (re-search-forward "^classdef") "C-e" "C-m" "C-i" "C-i"
  (insert "methods") "C-m"
  (insert "function out=foo") "C-m"
  (insert "end") "C-m"
  (insert "end") "C-m"
  (insert "end") "C-m"
  (re-search-backward "^classdef")
  (print (buffer-substring-no-properties (point) (point-max)))
  )
%}
classdef indent_xr_classdef1
