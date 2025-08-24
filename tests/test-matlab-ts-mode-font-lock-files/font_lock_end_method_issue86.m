% -*- matlab-ts -*-

% see https://github.com/acristoffers/tree-sitter-matlab/issues/86

classdef font_lock_end_method_issue86
    methods
        function endval = end(obj,k,n) % overload end() for indexing
            disp('in end')
            endval=1;
        end
    end
end
