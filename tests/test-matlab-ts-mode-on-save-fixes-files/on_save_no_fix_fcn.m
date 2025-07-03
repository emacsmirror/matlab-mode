% -*- matlab-ts -*-

% (t-utils-xr (rename-buffer "tmp__on_save_no_fix_fcn.m") (matlab-ts-mode-on-save-fix-name t))

% comment 2
function tmp__on_save_no_fix_fcn(a)
% c

    %% heading 1
    x = {'a', 'c()' 'b'};

    %% heading 2
    if a
        disp('foo');
    else
        disp('bar');
    end
end

function two
    disp('one')

    function goo
        disp('goo')
    end
end

