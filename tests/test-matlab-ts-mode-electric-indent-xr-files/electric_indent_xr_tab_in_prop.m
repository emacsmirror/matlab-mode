% -*- matlab-ts -*-

% Case1: (t-utils-xr (re-search-forward "param2") "C-a" "C-i")
classdef electric_indent_xr_tab_in_prop
    properties
        p1     (1,3)
          param2 (2,1)
             x{mustBeReal}
    end
end

% Case2: (t-utils-xr (re-search-backward "^classdef") (t-utils-xr-print-code (point) (re-search-forward "^end\n")))
