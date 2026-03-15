% -*- matlab-ts -*-

%{
  case1: 
  (t-utils-xr
  (re-search-forward "^ +p1")
  "C-i"
  "C-i"
  (t-utils-xr-print-code (re-search-backward "^fun") (point-max))
  )
%}

function electric_indent_xr_prop_hidden_nodes(p1,prop2)
    arguments (Input)
        p1   { }
        prop2  {   }
    end
    disp(p1)
    disp(prop2)
end
