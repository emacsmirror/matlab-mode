% -*- matlab-ts -*-

% Case1: (t-utils-xr "C-n" "C-e" "C-m" (insert "in1, ...") "C-i")
function [a, b] = indent_xr_fcn_args( ...
    a, b)
end

% Case2: (t-utils-xr (t-utils-xr-print-code (point-min) (point-max)))
