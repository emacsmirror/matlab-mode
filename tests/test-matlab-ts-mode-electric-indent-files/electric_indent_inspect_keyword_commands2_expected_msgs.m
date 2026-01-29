% -*- matlab-ts -*- %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

classdef electric_indent_inspect_keyword_commands2 %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
    methods (Static) %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
        function test %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>

            events('handle') %  <{Matched rule: ((parent-is "\\`function_definition\\'") parent matlab-ts-mode--set-function-indent-level-for-gp)}>
            foo1 = {events('handle')}; %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
            disp(foo1); %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>

            enumeration('matlab.OnOffSwitchSTate') %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
            foo2 = {enumeration('matlab.OnOffSwitchSTate')}; %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
            disp(foo2); %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>

            methods('double') %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
            foo3 = {methods('double')}; %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
            disp(foo3); %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>

        end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
    end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
