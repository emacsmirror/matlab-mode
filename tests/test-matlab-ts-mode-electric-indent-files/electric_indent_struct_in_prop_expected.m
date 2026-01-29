% -*- matlab-ts -*-

% t-utils-test-indent: no-line-by-line-indent - line-by-line typing results in error nodes

classdef electric_indent_struct_in_prop
    properties
        s (1,1) struct {mustBeNonempty} = struct( ...
            'field1'     , 1, ...
            'otherField2', 2);
        m = [100,   2
               3, 400];
    end

end
