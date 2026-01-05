% -*- matlab-ts -*-

%{
  Follow validates we do not trim trailing whitespace when electric indent is active and point is
  within an ERROR node.

  case1:
  (t-utils-xr

  (goto-char (point-max))

  (insert "switch a")   "C-m"
  "C-e"
  (insert "1")          "C-m"
  (insert "disp('1')")

  )
  
%}
