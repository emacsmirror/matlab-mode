% -*- matlab-ts -*-

function indent_xr_if_cond(a, b)

    % Case1: (t-utils-xr "C-n" "C-e" "C-m" (insert "b > 1") "C-m")
    if a > 1 && ...
    end

    % Case2: (t-utils-xr "C-n" "C-n" "C-i" (insert "b > 1"))
    switch a > 1 && ...

      case 1:
        disp('1');
    end

    % Case3: (t-utils-xr "C-n" "C-n" "C-i" (insert "b > 1") "C-i")
    while a > 1 && ...

    end

end

% Case4: (t-utils-xr (t-utils-xr-print-code (point-min) (point-max)))
