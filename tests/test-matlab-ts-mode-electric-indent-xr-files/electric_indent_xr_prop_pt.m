% -*- matlab-ts -*-

classdef electric_indent_xr_prop_pt
    % case1: (t-utils-xr (re-search-forward "p1") "C-i")
    properties
        p1(1,3)
           param2                (2,1)
        x{mustBeReal}
    end
    % case2: (t-utils-xr "C-a" (re-search-backward "param2") "C-a" "C-i")
end

% case3: (t-utils-xr (re-search-backward "^classdef") (t-utils-xr-print-code (point) (point-max)))
