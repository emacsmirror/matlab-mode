% -*- matlab-ts -*-

%{
  Case1:
  (t-utils-xr

  (re-search-forward "1,")
  "C-i"
  )
%}

if a
    a(   [1,    2], :) = ...                                                                  
        [1,          2,   3; ...
         100, 200, 300];
end
    
