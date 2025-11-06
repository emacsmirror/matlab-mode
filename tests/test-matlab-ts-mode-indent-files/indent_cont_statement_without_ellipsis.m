% -*- matlab-ts -*-
%
% t-utils-test-indent: no-line-by-line-indent - typing the incomplete expression line-by-line
% results in a parse tree that can't match below.
%
function a=indent_cont_statement_without_ellipsis
    % The matlab-tree-sitter, by design for simplicity treats "..." as comments so the following
    % is an error in MATLAB, but doesn't generate a parse error and indents the same as if
    % the ellipsis (...) were present.
    a =
        1 +
        2;
end
