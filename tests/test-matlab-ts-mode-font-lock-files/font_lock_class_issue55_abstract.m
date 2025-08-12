% -*- matlab-ts -*-

% https://github.com/acristoffers/tree-sitter-matlab/issues/55

classdef font_lock_class_issue55_abstract
    properties(Abstract)
        p1
    end

    methods(Abstract)
        a = f(obj);
        b = g(obj);
    end
end
