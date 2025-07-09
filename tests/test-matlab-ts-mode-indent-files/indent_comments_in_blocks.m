% -*- matlab-ts -*-
classdef indent_comments_in_blocks
    properties
        bar;
        % comment here
    end
    events
        e1
        % foo
        e2
        % bar
    end

    enumeration
        % foo
        one
        two
        % foo
    end 
    
    methods
        % comment here
        function foo(a)

            switch a
              % comment here
              case 1
                % comment here
              otherwise
                % comment here
                ;
            end
        end
    end
end
