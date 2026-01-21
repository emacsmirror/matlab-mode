% -*- matlab-ts -*-

classdef electric_indent_inspect_keyword_commands2
    methods (Static)
        function test

            events('handle')
            foo1 = {events('handle')};
            disp(foo1);

            enumeration('matlab.OnOffSwitchSTate')
            foo2 = {enumeration('matlab.OnOffSwitchSTate')};
            disp(foo2);

            methods('double')
            foo3 = {methods('double')};
            disp(foo3);

        end
    end
end
