% -*- matlab-ts -*-

% t-utils-test-indent: no-line-by-line-indent - line-by-line typing results in error nodes

classdef electric_indent_struct_and_matrix_in_prop
    properties
        s = struct( ...
         'field1', 1, ...
             'otherField2', 2);
        m = [100, 2
               3, 400];
    end

end
