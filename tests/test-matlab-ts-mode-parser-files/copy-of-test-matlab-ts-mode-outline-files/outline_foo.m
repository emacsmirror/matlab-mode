% -*- matlab-ts -*-
classdef outline_foo
    methods
        function one(a)
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
    end
end
