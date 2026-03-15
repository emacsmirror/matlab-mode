% -*- matlab-ts -*-

%{ 
  case1: 
  (t-utils-xr 
  (re-search-forward "\\.\\.\\.") 
  "C-a" 
  (delete-region (point) (point-min)) 
  (insert "\n\n\n")
  (indent-region (point-min) (point-max))
  (t-utils-xr-print-code (point-min) (point-max))
  )
%}


...

    % comment
