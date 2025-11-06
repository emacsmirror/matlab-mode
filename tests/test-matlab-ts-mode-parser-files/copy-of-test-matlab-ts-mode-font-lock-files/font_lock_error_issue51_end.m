% -*- matlab-ts -*-

% See https://github.com/acristoffers/tree-sitter-matlab/issues/51
% The 'end' should be recognized as a keyword.

function result = font_lock_error_issue51_end(a, b)
    result = 1 + myfcn(a, b) + ...

end
