% -*- matlab-ts -*-

% t-utils-test-indent: no-line-by-line-indent - when typing line-by-line we can't align the
% arguments because we don't have them all.

function electric_indent_arguments(a, param2, varargin)
    arguments
        a      {mustBeNumeric, mustBeReal, mustBeFinite}
        param2 {mustBeNumeric, mustBeReal, mustBeFinite}
    end
    arguments (Repeating)
        varargin
    end
    disp(a);
    disp(param2);
    disp(varargin);
end
