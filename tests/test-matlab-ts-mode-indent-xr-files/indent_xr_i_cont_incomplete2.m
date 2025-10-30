% -*- matlab-ts -*-
%
% See: https://github.com/acristoffers/tree-sitter-matlab/issues/51
%
%{
  Case1:
  (t-utils-xr

  (re-search-forward "^fun") "C-e"             "C-m"
  (insert "result = 1 + myfcn(a, b) + ...")    "C-m"
  (insert "b;")                                "C-i"

  (re-search-backward "^fun")
  (t-utils-xr-print-code (point) (point-max))

  )
%}
function result = indent_xr_i_cont_incomplete2(a, b)
end
