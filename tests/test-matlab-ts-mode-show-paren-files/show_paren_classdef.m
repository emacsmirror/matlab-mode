% -*- matlab-ts -*-

% Case1: (t-utils-xr "C-a" "C-n" (prin1 (matlab-ts-mode--show-paren-or-block)))
classdef show_paren_classdef < handle

    % Case2: (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
    properties
        foo;
        bar;
        % Case3: (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
    end

    % Case4: (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
    events
        goo;
        % Case5: (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
    end

    % Case6: (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
    methods

        % Case7: (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
        function foo(a)

            % Case8: (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
            arguments
                a
            % Case9: (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
            end

            % Case10: (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
            if a > 0
                disp('here')
                if a > 10
                    disp('a > 10');
                    % Case11: (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
                    switch a
                      % Case12: (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
                      case 11
                        disp('a == 11')
                        % Case13: (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
                        for idx=1:a
                            disp(idx);
                            % Case14: (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
                        end

                        % Case15: (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
                        parfor idx=1:a
                            disp(idx);
                            % Case16: (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
                        end
                        idx = 0
                        % Case17: (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
                        while idx < a
                            idx = idx + 1;
                            % Case18: (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
                        end
                        % Case19: (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
                      otherwise
                        disp('a > 11');
                        % Case20: (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
                    end
                    % Case21: (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
                elseif a > 11
                    disp('a > 11')
                    % Case22: (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
                else
                    disp('a <= 0');
                    % Case23: (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
                    try
                        error('foo');
                        % Case24: (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
                    catch ME
                        disp('caught error');
                        % Case25: (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
                    end
                end

                % Case26: (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
            elseif a == 0
                disp('a == 0');
            else
                disp('a < 0');
                % Case27: (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
            end
        % Case28: (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
        end
    % Case29: (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
    end

% Case30: (t-utils-xr "C-a" "C-n" (prin1 (matlab-ts-mode--show-paren-or-block)))
end
