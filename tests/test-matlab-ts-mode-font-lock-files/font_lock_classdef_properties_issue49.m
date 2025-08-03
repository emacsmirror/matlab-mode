% -*- matlab-ts -*-
%
% See: https://github.com/acristoffers/tree-sitter-matlab/issues/49
% where p1 double wasn't being parsed correctly.

classdef font_lock_classdef_properties_issue49
    properties
        p0
        p1 double;
    end
end
