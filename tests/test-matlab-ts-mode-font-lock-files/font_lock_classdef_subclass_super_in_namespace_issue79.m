% -*- matlab-ts -*-

% See https://github.com/acristoffers/tree-sitter-matlab/issues/79

classdef font_lock_classdef_subclass_super_in_namespace_issue79 < myUtils.super
    methods
        function obj=SubClass
            obj@myUtils.super;
            disp('in SubClass')
        end
    end
end
