% -*- matlab-ts -*-

% (t-utils-xr "C-a" "C-n" (prin1 (matlab-ts-mode--show-paren-or-block)))
classdef show_paren_classdef

    % (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
    properties
        foo;
        bar;
        % (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
    end

    % (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
    events
        goo;
        % (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
    end

    % (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
    methods

        % (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
        function foo(a)

            % (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
            arguments
                a
            % (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
            end

            % (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
            if a > 0
                disp('here')
                if a > 10
                    disp('a > 10');
                    % (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
                    switch a
                      % (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
                      case 11
                        disp('a == 11')
                        % (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
                        for idx=1:a
                            disp(idx);
                            % (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
                        end

                        % (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
                        parfor idx=1:a
                            disp(idx);
                            % (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
                        end
                        idx = 0
                        % (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
                        while idx < a
                            idx = idx + 1;
                            % (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
                        end
                        % (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
                      otherwise
                        disp('a > 11');
                        % (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
                    end
                    % (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
                elseif a > 11
                    disp('a > 11')
                    % (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
                else
                    disp('a <= 0');
                    % (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
                    try
                        error('foo');
                        % (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
                    catch ME
                        disp('caught error');
                        % (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
                    end
                end

                % (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
            elseif a == 0
                disp('a == 0');
            else
                disp('a < 0');
                % (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
            end
        % (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
        end
    % (t-utils-xr "C-a" "C-n" "M-m" (prin1 (matlab-ts-mode--show-paren-or-block)))
    end

% (t-utils-xr "C-a" "C-n" (prin1 (matlab-ts-mode--show-paren-or-block)))
end
