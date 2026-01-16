% -*- matlab-ts -*-

% t-utils-test-indent: no-line-by-line-indent - line-by-line typing results in error nodes

classdef electric_indent_method_attributes
    methods (Access =   private, Static)
        function b = foo(a)
b = 2 * a;
        end
    end
end
