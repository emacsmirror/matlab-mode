% -*- matlab-ts -*-

% (t-utils-xr "C-n" "C-e" "C-m" (insert "%c") "C-i")
classdef indent_xr_statement_body

    % (t-utils-xr "C-n" "C-e" "C-m" (insert "%c") "C-i")
    properties
    end

    % (t-utils-xr "C-n" "C-e" "C-m" (insert "%c") "C-i")
    methods
        function foo(a)

            % (t-utils-xr "C-n" "C-e" "C-m" (insert "%c") "C-i")
            if a == 1
                % (t-utils-xr "C-n" "C-e" "C-m" (insert "%c") "C-i")
                try
                    % (t-utils-xr "C-n" "C-e" "C-m" (insert "%c") "C-i")
                catch
                end

                % (t-utils-xr "C-n" "C-e" "C-m" (insert "%c") "C-i")
                spmd
                end

                % (t-utils-xr "C-n" "C-e" "C-m" (insert "%c") "C-i")
            elseif a == 2

                % (t-utils-xr "C-n" "C-e" "C-m" (insert "%c") "C-i")
            else
                
            end

            % (t-utils-xr "C-n" "C-e" "C-m" (insert "%c") "C-i")
            while a
            end

            % (t-utils-xr "C-n" "C-e" "C-m" (insert "%c") "C-i")
            for fIdx = 1:10
            end
        end
    end

    % (t-utils-xr "C-n" "C-e" "C-m" (insert "%c") "C-i")
    events
    end

    % (t-utils-xr "C-n" "C-e" "C-m" (insert "%c") "C-i")
    enumeration
        red
    end
end

% (t-utils-xr (re-search-backward "^classdef") (print (buffer-substring-no-properties (point) (point-max))))
