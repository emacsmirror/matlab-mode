% -*- matlab-ts -*-

% See: See https://github.com/acristoffers/tree-sitter-matlab/issues/47
%
% We want a RET "C-m" on the "outResult = longFunction(1, ..." line to indent.
% Likewise for a TAB "C-i" on the line.
%
% Case1: (t-utils-xr "C-n" "C-n" "C-e" "C-m" (insert "b);") (re-search-backward "^fun") (t-utils-xr-print-code (point) (point-max)))
function outResult = indent_xr_fun2(b)
    outResult = longFunction(1, ...
end
