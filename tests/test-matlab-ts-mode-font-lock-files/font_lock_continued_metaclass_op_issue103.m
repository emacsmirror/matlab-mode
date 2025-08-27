% -*- matlab-ts -*-

% see https://github.com/acristoffers/tree-sitter-matlab/issues/103

classdef font_lock_continued_metaclass_op_issue103
    properties (GetAccess = {?foo.bar...
                             .C})
        p1
    end
end

