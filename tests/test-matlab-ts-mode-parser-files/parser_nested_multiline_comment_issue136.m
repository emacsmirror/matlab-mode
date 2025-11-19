% -*- matlab-ts -*-

% See https://github.com/acristoffers/tree-sitter-matlab/issues/136

%{
  a * b = 1;
  %{
      c * d = 1;
  %}
  e * f = 1;
%}

%{
  this is a one multline comment containing two text lines
  "%}"
%}
