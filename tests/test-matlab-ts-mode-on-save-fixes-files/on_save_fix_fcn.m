% -*- matlab-ts -*-

% (t-utils-xr (rename-buffer "tmp__on_save_fix_fcn.m") (matlab-ts-mode-on-save-fix-name t))

function foo
    disp('foo')
    bar;
end

function bar
    disp('bar')
end
