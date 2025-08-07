% -*- matlab-ts -*-

function indent_xr_if_cond(a, b)

    % (t-utils-xr "C-n" "C-e" "C-m" (insert "b > 1"))
    if a > 1 && ...
    end

    % (t-utils-xr "C-n" "C-n" "C-i" (insert "b > 1"))
    switch a > 1 && ...

      case 1:
        disp('1');
    end

    % (t-utils-xr "C-n" "C-n" "C-i" (insert "b > 1"))
    while a > 1 && ...

    end

end

% (t-utils-xr (t-utils-xr-print-code (point-min) (point-max)))
