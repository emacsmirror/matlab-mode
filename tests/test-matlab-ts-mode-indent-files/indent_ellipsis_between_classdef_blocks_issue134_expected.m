% -*- matlab-ts -*-

% See: https://github.com/acristoffers/tree-sitter-matlab/issues/134

classdef indent_ellipsis_between_classdef_blocks_issue134
    properties
        p1
    end
    ...
    methods
        function foo(~)

        end
    end
end
