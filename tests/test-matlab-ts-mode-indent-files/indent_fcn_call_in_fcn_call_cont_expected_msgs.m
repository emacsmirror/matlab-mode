% -*- matlab-ts -*- %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

classdef indent_fcn_call_in_fcn_call_cont %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
    properties (Constant) %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
        tabularDs = tabularTextDatastore(some.namespace.someFunction( ... %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
                                             'argument1', 'argument2', 'argument3'), ... %  <{Matched rule: ((n-p-gp "\\`arguments\\'" "\\`function_call\\'" "\\`field_expression\\'") grand-parent 4)}>
                                         "TreatAsMissing", "NA", "MissingValue", 0, ... %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
                                         "SelectedVariableNames", {'ArrTime', 'DepDelay'}); %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
    end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
