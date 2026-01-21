% -*- matlab-ts -*-

% t-utils-test-indent: no-line-by-line-indent - line-by-line typing results in error nodes

classdef electric_indent_prop2
    properties
        abc = true;
        fubar;
        b      = false;
        p1
        lonPropertyName   (1,1) double = 1.1;
    end
end
