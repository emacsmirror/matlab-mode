% -*- matlab-ts -*-

% See: https://github.com/acristoffers/tree-sitter-matlab/issues/113

function y = indent_multiple_arg_blocks_issue113(a, b, varargin)
    arguments
        a uint32
        b uint32
    end
    arguments (Repeating)
        varargin
    end

    y = a + b + length(varargin);

end
