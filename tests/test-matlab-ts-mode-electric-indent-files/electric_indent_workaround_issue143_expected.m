% -*- matlab-ts -*-

% workaround https://github.com/acristoffers/tree-sitter-matlab/issues/143

if 1
    a = 1234 ./ 2 + 500 ./ 4 + 600.' + 700 .\ 1 + 800 .* 2 + sum([444., 555.]);
end
