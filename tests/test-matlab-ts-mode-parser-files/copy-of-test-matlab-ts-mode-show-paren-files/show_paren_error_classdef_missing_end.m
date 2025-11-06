% -*- matlab-ts -*-
%
% Case1: (t-utils-xr "C-a" "C-n" (prin1 (matlab-ts-mode--show-paren-or-block)))
classdef show_paren_error_classdef_missing_end
    methods
        function foo
        end
    end
