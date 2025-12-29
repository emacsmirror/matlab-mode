% -*- matlab-ts -*- %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

function [pass, requirements] = electric_indent_strings(model) %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

    s.foo = "foo"; %  <{Matched rule: ((parent-is "\\`function_definition\\'") parent matlab-ts-mode--set-function-indent-level-for-gp)}>
    s.(fieldName); %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>

    requirements = {message('a:b:c:foo').getString()}; %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>

    b = find_system(model, 'Name', 'Foo'); %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>

    if strcmp(get_param(b, 'BlockType'), 'Abs') %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
        pass = 'yes'; %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
    else %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
        pass = 'no'; %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
    end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>

end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
