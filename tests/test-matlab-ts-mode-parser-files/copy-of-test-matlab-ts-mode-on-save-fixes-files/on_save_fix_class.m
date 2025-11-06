% -*- matlab-ts -*-

% Case1: (t-utils-xr (rename-buffer "tmp__on_save_fix_class.m") (matlab-ts-mode-on-save-fix-name t))

classdef foo
    methods

        function bar
            disp('bar')
        end
    end
end
