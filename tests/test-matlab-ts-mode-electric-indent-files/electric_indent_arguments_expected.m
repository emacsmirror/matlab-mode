% -*- matlab-ts -*-
function electric_indent_arguments(in1, in2, varargin)
    arguments
        in1 {mustBeNumeric, mustBeReal, mustBeFinite}
        in2 {mustBeNumeric, mustBeReal, mustBeFinite}
    end
    arguments (Repeating)
        varargin
    end
    disp(in1);
    disp(in2);
    disp(varargin);
end
