% -*- matlab-ts -*-

% see https://github.com/acristoffers/tree-sitter-matlab/issues/80
% Given the following where the "blank" 3rd line contains 8 spaces we get a parse error

classdef font_lock_classdef_blank_line_issue80
    methods (Access=protected)
        
        blk = setValue_(blk, Value)
    end
end
