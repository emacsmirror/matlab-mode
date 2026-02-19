% -*- matlab-ts -*-

classdef electric_ends_cases

    % Case1: (t-utils-xr "C-n" "C-i" (insert "properties") "C-m" (insert "p1"))

    % Case2: (t-utils-xr "C-n" "C-i" (insert "methods") "C-m" (insert "% methods-comment"))

    % Case3: (t-utils-xr "C-n" "C-i" (insert "events") "C-m" (insert "% events-comment"))

    methods

        % Case4: (t-utils-xr "C-n" "C-i" (insert "function bar") "C-m" (insert "disp('bar')"))

        % Case5: (t-utils-xr (re-search-forward "foo help") "C-m" "C-e" (insert "line 2"))
        function foo(a)
        % foo help

            % Case6: (t-utils-xr "C-n" "C-i" (insert "arguments") "C-m" (insert "a"))

            % Case7: (t-utils-xr "C-n" "C-i" (insert "if a") "C-m" (insert "disp('if')"))

            % Case8: (t-utils-xr "C-n" "C-i" (insert "switch a") "C-m" (insert "1") "C-m" (insert "disp('case 1')"))

            % Case9: (t-utils-xr "C-n" "C-i" (insert "while true") "C-m" (insert "break"))

            % Case10: (t-utils-xr "C-n" "C-i" (insert "for idx=1:a") "C-m" (insert "disp(idx)"))

            % Case11: (t-utils-xr "C-n" "C-i" (insert "parfor idx=1:a") "C-m" (insert "disp(idx)"))

            % Case12: (t-utils-xr "C-n" "C-i" (insert "spmd") "C-m" (insert "q = magic(spmdIndex + 2)"))

            % Case13: (t-utils-xr "C-n" "C-i" (insert "try") "C-m" (insert "disp('try')"))

        end
    end

end

% Case14: (t-utils-xr (t-utils-xr-print-code (point-min) (point-max)))

