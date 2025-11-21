% -*- matlab-ts -*-

% See: https://github.com/acristoffers/tree-sitter-matlab/issues/135

% t-utils-test-indent: no-line-by-line-indent - multi-line comment below can not be indented
% line-by-line


a = 1;
b = 2;

if a == 10 || ...
   %{
     block
     comment
   %}
   b == 2
    disp('here')
end
