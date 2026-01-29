% -*- matlab-ts -*-

% t-utils-test-indent: no-line-by-line-indent - line-by-line typing results in error nodes

classdef electric_indent_m_matrix_in_prop
 properties
     pStrings string = [    ... Default value
                % comment1
                "string one"; ...
        
                % comment2
                "string2"; ...
        
                % comment for following two strings
                "string3"; ...
                           "string four that is long"; ...
            ];
end
end
