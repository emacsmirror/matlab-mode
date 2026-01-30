% -*- matlab-ts -*-

% t-utils-test-indent: no-line-by-line-indent - line-by-line typing results in error nodes

classdef electric_indent_prop_attribute_id_cell_value
    properties (SetAccess = 'private', GetAccess = {?foo.barj.zoo, ?foo.bar.goo})
        ConnectionHandle
    end
end
