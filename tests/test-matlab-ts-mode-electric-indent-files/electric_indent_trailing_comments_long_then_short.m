% -*- matlab-ts -*-

% t-utils-test-indent: no-line-by-line-indent - when we type line-by-line, we don't see later
%                      assignment values, thus to fully indent, need to re-indent the file after 
%                      typing line-by-line.

% When computing trailing comment location, need to have lines electric indented including
% assignment and properties

classdef electric_indent_trailing_comments_long_then_short
    properties
        longProp = 1;         % comment
        x  = [1, 2, 3,4, 5, 6, 7, 8, 9, 10]; % comment2
    end

    methods (Static)
        function y = foo
            longVarName = 1;  % comment1
            x = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]; % comment2

            y = longVarName + x;
        end

    end

end
