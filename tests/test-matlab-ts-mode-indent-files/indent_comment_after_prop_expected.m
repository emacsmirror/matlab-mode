% -*- matlab-ts -*-

% comment
classdef indent_comment_after_prop < handle

    % comment
    properties
        foo;
        bar;
        % comment
    end

    % comment
    events
        goo;
        % comment
    end

    % comment
    methods

        % comment
        function foo1(a)

            % comment
            arguments
                a
                % comment
            end

            % comment
            if a > 0
                disp('here')
                if a > 10
                    disp('a > 10');
                    % comment
                    switch a
                      % comment
                      case 11
                        disp('a == 11')
                        % comment
                        for idx=1:a
                            disp(idx);
                            % comment
                        end

                        % comment
                        parfor idx=1:a
                            disp(idx);
                            % comment
                        end
                        idx = 0
                        % comment
                        while idx < a
                            idx = idx + 1;
                            % comment
                        end
                        % comment
                      otherwise
                        disp('a > 11');
                      % comment
                    end
                    % comment
                elseif a > 11
                    disp('a > 11')
                    % comment
                else
                    disp('a <= 0');
                    % comment
                    try
                        error('foo');
                        % comment
                    catch ME
                        disp('caught error');
                        % comment
                    end
                end

                % comment
            elseif a == 0
                disp('a == 0');
            else
                disp('a < 0');
                % comment
            end
            % comment
        end
        % comment
    end

    % comment
end
