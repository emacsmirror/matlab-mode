% -*- matlab-ts -*-

classdef electric_ends_cases

    %(t-utils-xr "C-n" "C-i" (insert "properties") "C-m" (insert "p1"))

    %(t-utils-xr "C-n" "C-i" (insert "methods") "C-m" (insert "% methods-comment"))

    %(t-utils-xr "C-n" "C-i" (insert "events") "C-m" (insert "% events-comment"))

    methods

        %(t-utils-xr "C-n" "C-i" (insert "function bar") "C-m" (insert "disp('bar')"))

        function foo(a)

            %(t-utils-xr "C-n" "C-i" (insert "arguments") "C-m" (insert "a"))

            %(t-utils-xr "C-n" "C-i" (insert "if a") "C-m" (insert "disp('if')"))

            %(t-utils-xr "C-n" "C-i" (insert "switch a") "C-m" "C-e" (insert "1") "C-m" (insert "disp('case 1')"))

            %(t-utils-xr "C-n" "C-i" (insert "while true") "C-m" (insert "break"))

            %(t-utils-xr "C-n" "C-i" (insert "for idx=1:a") "C-m" (insert "disp(idx)"))

            %(t-utils-xr "C-n" "C-i" (insert "parfor idx=1:a") "C-m" (insert "disp(idx)"))

            %(t-utils-xr "C-n" "C-i" (insert "spmd") "C-m" (insert "q = magic(spmdIndex + 2)"))

            %(t-utils-xr "C-n" "C-i" (insert "try") "C-m" (insert "disp('try')"))

        end
    end

end

%(t-utils-xr (t-utils-xr-print-code (point-min) (point-max)))

