% -*- matlab-ts -*-

% t-utils-test-indent: no-line-by-line-indent - line-by-line typing results in error nodes

classdef electric_indent_classdef_prop_get_set
    properties
        inputMatrix = [1, 0; 0, 1]
    end

    methods
        function obj = set.inputMatrix(obj, val)
            try chol(val)
                obj.inputMatrix = val;
            catch ME
                error("inputMatrix must be symmetric positive definite.")
            end
        end

        function m = get.inputMatrix(obj)
            m = obj.inputMatrix;
        end
    end
end
