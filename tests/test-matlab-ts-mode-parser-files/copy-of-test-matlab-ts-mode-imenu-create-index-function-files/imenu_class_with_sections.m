% -*- matlab-ts -*-

classdef imenu_class_with_sections
% my class
    properties
        a
        b
        c
        d
    end

    methods
        function r=foo1
        % in foo1
        %

            %%
            r = a + b;

            %%
            r = r + c * d;
        end

        function foo2(obj, a, b)
        % in foo2
        % 

            %% foo2 - section1
            obj.a = a;
            obj.b = b;

            %% foo2 - section2
            obj.c = a * 2;
            obj.c = a * 3;
            
        end

    end

end

