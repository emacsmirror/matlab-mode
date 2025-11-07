% -*- matlab-ts -*-

% See https://github.com/acristoffers/tree-sitter-matlab/issues/121


y = ones(1,100);
parfor (i = 1:100, myutils.maxWorkers)
    y(i) = i;
end
