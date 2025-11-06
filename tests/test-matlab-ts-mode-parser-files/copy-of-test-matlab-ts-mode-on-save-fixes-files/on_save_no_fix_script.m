% -*- matlab-ts -*-

% Case1: (t-utils-xr (rename-buffer "tmp__on_save_fix_script.m") (matlab-ts-mode-on-save-fix-name t))

% Calling foo makes this a script and which uses local functions.
foo

function foo
    disp('foo')
    bar;
end

function bar
    disp('bar')
end
