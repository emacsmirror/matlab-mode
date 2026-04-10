% -*- matlab-ts -*-

%{ 
  Case1: 
  (t-utils-xr 

  (re-search-forward "x=2;")
  (indent-region (save-excursion (re-search-backward "^a")) (point))

  (t-utils-xr-print-code (save-excursion (re-search-backward "^a")) (point-max))
  )
%}

a=1;
bVar = [1 2 300   ];
cLongVar = 10+f(3,4,    5)  ;
x=2;
