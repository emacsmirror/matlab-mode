% -*- matlab-ts -*- %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

function indent_parens(inputArgument1) %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
    if ((someNamespace.getStartValue(((inputArgument1 + someOtherFunction(b)) * 2 - ... %  <{Matched rule: ((parent-is "\\`function_definition\\'") parent matlab-ts-mode--set-function-indent-level-for-gp)}>
                                      offset))) > ... %  <{Matched rule: ((parent-is "\\`binary_operator\\'") parent 0)}>
        2 * (someOtherLongFunctionInANamespaceThatGetsEndValue.myFcn(inputArgument1, ... %  <{Matched rule: ((parent-is "\\`\\(?:\\(?:boolea\\|compariso\\)n_operator\\)\\'") parent 0)}>
                                                                     'end') + ... %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
             100)) %  <{Matched rule: ((parent-is "\\`binary_operator\\'") parent 0)}>
        disp('here') %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
    end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
