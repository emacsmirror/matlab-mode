% -*- matlab-ts -*-

% See: https://github.com/acristoffers/tree-sitter-matlab/issues/150

% t-utils-test-indent: no-line-by-line-indent - line-by-line typing results in error nodes

classdef electric_indent_methods_and_fcn_on_same_line_issue150
    methods function b = foo(a)
                b = 2 * a;
            end
    end
end
