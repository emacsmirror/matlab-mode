% -*- matlab-ts -*-

% See https://github.com/acristoffers/tree-sitter-matlab/issues/128

function out = font_lock_comment_between_arg_blocks_issue128(in1, in2)
    arguments (Input)
        in1 double
    end

    % comment
    arguments (Input)
        in2 double
    end

    out = in1 + in2;
end
