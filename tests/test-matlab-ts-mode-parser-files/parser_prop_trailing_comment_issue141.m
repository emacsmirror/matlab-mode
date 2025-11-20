% -*- matlab-ts -*-

% See https://github.com/acristoffers/tree-sitter-matlab/issues/141

classdef parser_prop_trailing_comment_issue141

    properties
        p2 = ...
            0

        % comment here

        %{
          foo
        %}

    end

end
