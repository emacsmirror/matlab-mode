% -*- matlab-ts -*-
% https://github.com/acristoffers/tree-sitter-matlab/issues/25
parpool(3)
spmd
    q = magic(spmdIndex + 2);
end
