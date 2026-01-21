% -*- matlab-ts -*- %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

% t-utils-test-indent: no-line-by-line-indent - when typing line-by-line we can't align the %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
% arguments because we don't have them all. %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher matlab-ts-mode--i-block-comment-end-anchor 0)}>

function electric_indent_arguments2( ... %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
    firstToAnalyze, argTwo, generateFoobar, NameValues) %  <{Matched rule: (matlab-ts-mode--i-fcn-args-next-line-matcher matlab-ts-mode--i-fcn-args-next-line-anchor matlab-ts-mode--i-fcn-args-next-line-offset)}>
    arguments %  <{Matched rule: ((parent-is "\\`function_definition\\'") parent matlab-ts-mode--set-function-indent-level-for-gp)}>
        firstToAnalyze string {mustBeScalarOrEmpty} %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
        argTwo { ... %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
                 Aero.internal.validation.mustBeEmptyStringOrStateSpace ... %  <{Matched rule: (matlab-ts-mode--i-validation-functions-matcher parent matlab-ts-mode--i-validation-functions-offset)}>
               } = '' %  <{Matched rule: ((n-p-gp "\\`}\\'" "\\`validation_functions\\'" nil) parent 0)}>
        generateFoobar            (1,1) matlab.lang.OnOffSwitchState = "off" %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
        NameValues.SecondDocument (1,1) string {mustBeMember(NameValues.SecondDocument, ["Document1", "Document2"])} = "Document1" %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
        NameValues.Level          (1,1) string {mustBeMember(NameValues.Level, ... %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
                                                             ["Lowest", "All", "1", "2", "3"])} = "Lowest" %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
    end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>

    disp(firstToAnalyze); %  <{Matched rule: ((parent-is "\\`function_definition\\'") parent matlab-ts-mode--set-function-indent-level-for-gp)}>
    disp(argTwo); %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
    disp(generateFoobar); %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
    disp(NameValues); %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
