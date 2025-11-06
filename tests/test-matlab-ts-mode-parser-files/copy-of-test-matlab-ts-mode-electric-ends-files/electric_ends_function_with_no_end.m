% -*- matlab-ts -*-

function electric_ends_function_with_no_end
disp('in electric_ends_function_with_no_end')
foo1();

% Case1: (t-utils-xr "C-n" (insert "function foo1") "C-m" (insert "disp('foo1')\n") (t-utils-xr-print-code (point-min) (point-max)))
