% -*- matlab-ts -*-

% See https://github.com/acristoffers/tree-sitter-matlab/issues/135

% Placing a single line comment after a line continuation is NOT valid in MATLAB, but using a
% single-line comment is valid.  See ./parser_multiline_comment_after_ellipsis_error_issue135.m

function b = parser_single_line_comment_after_ellipsis_error_issue135(a)
    b = ...
        % comment
        a * 2;
end
