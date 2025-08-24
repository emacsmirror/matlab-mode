% -*- matlab-ts -*-

% see https://github.com/acristoffers/tree-sitter-matlab/issues/92

if YL' * sdiff < (eps*norm(HESS,'fro'))
    x = 1;
end
