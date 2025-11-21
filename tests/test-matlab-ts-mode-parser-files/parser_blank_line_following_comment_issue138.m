% -*- matlab-ts -*-

% See https://github.com/acristoffers/tree-sitter-matlab/issues/138

% Below the 'blank' line after the comment contains 4 spaces and they
% shouldn't be part of the comment node.

function parser_blank_line_following_comment_issue138
    %{
      comment
    %}
    
end
