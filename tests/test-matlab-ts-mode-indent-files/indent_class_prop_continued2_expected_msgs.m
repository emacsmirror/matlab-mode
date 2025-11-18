% -*- matlab-ts -*- %  <{Matched rule: (maltab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

% t-utils-test-indent: no-line-by-line-indent - typing line by line doesn't work when there are no 'end' statements for classdef/properties %  <{Matched rule: (maltab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

classdef indent_class_prop_continued2 %  <{Matched rule: (maltab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

    properties (Constant) %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
        property1 = containers.Map(... %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
            { %  <{Matched rule: (matlab-ts-mode--i-arg-namespace-fcn-prop-matcher matlab-ts-mode--i-arg-namespace-fcn-prop-anchor 4)}>
              'one' %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
              'two' %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
            } ... %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>
            , ... %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
            { %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
              'foo' %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
              'bar' %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
            }); %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>

        property2 = someFcn(... %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
            { %  <{Matched rule: ((n-p-gp "\\`\\(?:)\\|arguments\\|line_continuation\\)\\'" "\\`function_call\\'" "\\`default_value\\'") great-grand-parent 4)}>
              'one' %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
              'two' %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
            } ... %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>
            , ... %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
            { %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
              'foo' %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
              'bar' %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
            }); %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>
    end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
