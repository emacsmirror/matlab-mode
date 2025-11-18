% -*- matlab-ts -*- %  <{Matched rule: (maltab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

function d=indent_namespace_fcn_continued(a, b) %  <{Matched rule: (maltab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

    try %  <{Matched rule: ((parent-is "\\`function_definition\\'") parent matlab-ts-mode--set-function-indent-level-for-gp)}>
        d = [a;b]; %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
    catch %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
        disp('[a;b] failed'); %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
    end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>

    % Function arguments are aligned when the first argument follows the open parenthesis %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
    someNamespace1.subNamespace2.myFunction(a, ... % comment for param1 %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
                                            b);    % comment for param2 %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>


    % Function arguments are indented and aligned when the first argument is on the next line: %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
    someNamespace1.subNamespace2.myFunction( ... %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
        a, ... % comment for param1 %  <{Matched rule: ((n-p-gp "\\`arguments\\'" "\\`function_call\\'" "\\`field_expression\\'") grand-parent 4)}>
        b);    % comment for param2 %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>

end % this function has an end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
