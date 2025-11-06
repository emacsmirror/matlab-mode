% -*- matlab-ts -*-

% see https://github.com/acristoffers/tree-sitter-matlab/issues/98

% see "Overload end for Classes"
% https://www.mathworks.com/help/matlab/matlab_oop/overload-end.html?s_tid=ai_sources_2_end+method

classdef (Abstract) font_lock_classdef_end_override_super
    methods
        ind = end(obj, k, n);
    end
end
