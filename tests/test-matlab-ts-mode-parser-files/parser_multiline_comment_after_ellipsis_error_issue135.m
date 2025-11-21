% -*- matlab-ts -*-

% See https://github.com/acristoffers/tree-sitter-matlab/issues/135

% Placing a multiline comment after a line continuation is valid in MATLAB, but using a single-line
% comment is not.  See ./parser_singleline_comment_after_ellipsis_error_issue135.m

function b = parser_multiline_comment_after_ellipsis_error_issue135(a)
    b = ...
        %{
          multiline line
          comment
        %}
        a * 2;
end
