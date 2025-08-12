% -*- matlab-ts -*-

% https://github.com/acristoffers/tree-sitter-matlab/issues/55

classdef font_lock_class_issue55_subclass < font_lock_class_issue55_abstract
    properties
        p1 = 10
    end

    methods
        function a = f(obj)
            a = obj.p1 + 10;
        end

        function b = g(obj)
            b = obj.p1 + 20;
        end
    end
end
