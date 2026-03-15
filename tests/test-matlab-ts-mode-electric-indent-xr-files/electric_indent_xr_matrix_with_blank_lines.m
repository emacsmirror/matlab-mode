% -*- matlab-ts -*-

%{
  Case1:
  (t-utils-xr 
  (re-search-forward "30,")
  "C-i"
  )
%}

mat1 = [     111111, 2
         
        30,4
         500, 4];

%{
  Case2:
  (t-utils-xr 
  (re-search-forward "30,")
  "C-i"
  )
%}

mat2 = [     11, 2
         
               30,4


   5000000000, 4];

% Case3: (t-utils-xr (indent-region (point-min) (point-max))  (t-utils-xr-print-code (point-min) (point-max)))
