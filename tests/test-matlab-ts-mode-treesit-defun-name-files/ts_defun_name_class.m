% -*- matlab-ts -*-
classdef ts_defun_name_class
    properties
        Value {mustBeNumeric}
    end
    methods
        function r = roundOff(obj)
            r = round([obj.Value], 2);
        end
        function r = multiplyBy(obj, n)
            r = [obj.Value] * n;
        end
    end
end
