% -*- matlab-ts -*-

% See https://github.com/acristoffers/tree-sitter-matlab/issues/132

classdef font_lock_one_liner_empty_methods_issue132

    methods
        function obj = emptyMethods end

        function Empty2 end

        function Empty3() end

        function Empty4(in) end
    end
end
