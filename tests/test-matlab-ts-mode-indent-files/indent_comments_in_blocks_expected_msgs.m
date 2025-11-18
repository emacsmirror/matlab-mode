% -*- matlab-ts -*- %  <{Matched rule: (maltab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
classdef indent_comments_in_blocks < handle %  <{Matched rule: (maltab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
    properties %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
        bar; %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
        % comment here %  <{Matched rule: ((parent-is "\\`\\(?:arguments_statement\\|e\\(?:numeration\\|vents\\)\\|f\\(?:or_statement\\|unction_definition\\)\\|if_statement\\|methods\\|properties\\|while_statement\\)\\'") parent 4)}>
    end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
    events %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
        e1 %  <{Matched rule: ((parent-is "\\`\\(?:arguments_statement\\|e\\(?:numeration\\|vents\\)\\|f\\(?:or_statement\\|unction_definition\\)\\|if_statement\\|methods\\|properties\\|while_statement\\)\\'") parent 4)}>
        % foo %  <{Matched rule: ((parent-is "\\`\\(?:arguments_statement\\|e\\(?:numeration\\|vents\\)\\|f\\(?:or_statement\\|unction_definition\\)\\|if_statement\\|methods\\|properties\\|while_statement\\)\\'") parent 4)}>
        e2 %  <{Matched rule: ((parent-is "\\`\\(?:arguments_statement\\|e\\(?:numeration\\|vents\\)\\|f\\(?:or_statement\\|unction_definition\\)\\|if_statement\\|methods\\|properties\\|while_statement\\)\\'") parent 4)}>
        % bar %  <{Matched rule: ((parent-is "\\`\\(?:arguments_statement\\|e\\(?:numeration\\|vents\\)\\|f\\(?:or_statement\\|unction_definition\\)\\|if_statement\\|methods\\|properties\\|while_statement\\)\\'") parent 4)}>
    end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>

    enumeration %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
        % foo %  <{Matched rule: ((parent-is "\\`\\(?:arguments_statement\\|e\\(?:numeration\\|vents\\)\\|f\\(?:or_statement\\|unction_definition\\)\\|if_statement\\|methods\\|properties\\|while_statement\\)\\'") parent 4)}>
        one %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
        two %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
        % foo %  <{Matched rule: ((parent-is "\\`\\(?:arguments_statement\\|e\\(?:numeration\\|vents\\)\\|f\\(?:or_statement\\|unction_definition\\)\\|if_statement\\|methods\\|properties\\|while_statement\\)\\'") parent 4)}>
    end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>

    methods %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
        % comment here %  <{Matched rule: ((parent-is "\\`\\(?:arguments_statement\\|e\\(?:numeration\\|vents\\)\\|f\\(?:or_statement\\|unction_definition\\)\\|if_statement\\|methods\\|properties\\|while_statement\\)\\'") parent 4)}>
        function foo(a) %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>

            switch a %  <{Matched rule: ((parent-is "\\`function_definition\\'") parent matlab-ts-mode--set-function-indent-level-for-gp)}>
              % comment here %  <{Matched rule: ((parent-is "\\`\\(?:case_clause\\|otherwise_clause\\|switch_statement\\)\\'") parent 2)}>
              case 1 %  <{Matched rule: ((node-is "\\`\\(?:\\(?:ca\\|otherwi\\)se_clause\\)\\'") parent 2)}>
                % comment here %  <{Matched rule: ((parent-is "\\`\\(?:case_clause\\|otherwise_clause\\|switch_statement\\)\\'") parent 2)}>
              otherwise %  <{Matched rule: ((node-is "\\`\\(?:\\(?:ca\\|otherwi\\)se_clause\\)\\'") parent 2)}>
                % comment here %  <{Matched rule: ((parent-is "\\`\\(?:case_clause\\|otherwise_clause\\|switch_statement\\)\\'") parent 2)}>
                ; %  <{Matched rule: ((parent-is "\\`\\(?:case_clause\\|otherwise_clause\\|switch_statement\\)\\'") parent 2)}>
            end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
        end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
    end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
