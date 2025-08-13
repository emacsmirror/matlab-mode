% -*- matlab-ts -*-

% https://github.com/acristoffers/tree-sitter-matlab/issues/55

classdef font_lock_class_issue55_abstract
    properties(Abstract)
        p1
    end

    methods(Abstract)
        a = foo(obj);
        b = goo(obj);
    end
end
