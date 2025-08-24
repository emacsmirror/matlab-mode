% -*- mode: matlab-ts; matlab-ts-mode-electric-ends: nil -*-

classdef indent_classdef_abs_methods
    methods(Abstract)
        % (t-utils-xr "C-n" "C-e" "C-m" "C-a" "C-i" "C-a" "C-k" "C-o" "C-i" (insert "%comment"))
        [area, desc] = getArea(obj)
    end
end

% (t-utils-xr (t-utils-xr-print-code (point-min) (point-max)))
