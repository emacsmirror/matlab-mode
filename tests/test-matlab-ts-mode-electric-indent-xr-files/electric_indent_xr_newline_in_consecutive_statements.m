% -*- matlab-ts -*-

%{ 
  Case1: 
  (t-utils-xr 
  (re-search-forward "x1")
  "C-a" "C-i"
  (re-search-backward "^if") (t-utils-xr-print-code (point) (re-search-forward "end\n"))
  (re-search-backward "^    x1")
  "C-a" "C-b" "C-m"
  (re-search-backward "^if") (t-utils-xr-print-code (point) (re-search-forward "end\n"))
  )
  
%}

if x
var1 = [1,2];
longVar2 = 3;
x1    =                   4;
x23       = 5      ;
end
