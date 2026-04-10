% -*- matlab-ts -*-

%{ 
  Case1: 
  (t-utils-xr 

  (re-search-forward "^a")
  (backward-char)
  (indent-region (point) (point-max))

  (t-utils-xr-print-code (save-excursion (re-search-forward "^a") (pos-bol)) (point-max))
  )
%}

a=1;
bVar = [1 2 300   ];
cLongVar = 10+f(3,4,    5)  ;
x=2;
