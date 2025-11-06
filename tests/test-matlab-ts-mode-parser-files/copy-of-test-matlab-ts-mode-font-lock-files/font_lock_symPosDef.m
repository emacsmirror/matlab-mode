% -*- matlab-ts -*-
classdef font_lock_symPosDef
    properties
        inputMatrix = [1 0; 0 1]
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
