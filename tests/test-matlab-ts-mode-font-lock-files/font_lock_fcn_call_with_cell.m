% See comment in https://github.com/acristoffers/tree-sitter-matlab/issues/53
% This shouldn't be a parse error.

myfcn({'str'});

function myfcn(in)
    disp(in)
end

