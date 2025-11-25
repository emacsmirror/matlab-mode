% -*- matlab-ts -*- %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

% t-utils-test-indent: no-line-by-line-indent - continued "fcn_call(arg1, ..." %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
% isn't identified as a function call, so indenting line by line fails on this. %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher parent 0)}>

function indent_if_continued %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

    if condition1 || ... %  <{Matched rule: ((parent-is "\\`function_definition\\'") parent matlab-ts-mode--set-function-indent-level-for-gp)}>
       condition2 || ... %  <{Matched rule: ((parent-is "\\`\\(?:\\(?:boolea\\|compariso\\)n_operator\\)\\'") parent 0)}>
       fcn_call(arg1, ... %  <{Matched rule: ((parent-is "\\`\\(?:\\(?:boolea\\|compariso\\)n_operator\\)\\'") parent 0)}>
                arg2) %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>

        line_in_if(); %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>

    elseif condition1 + condition2 == ... %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
           2770000 || ... %  <{Matched rule: ((parent-is "\\`\\(?:\\(?:boolea\\|compariso\\)n_operator\\)\\'") parent 0)}>
           fcn_call(arg1, ... %  <{Matched rule: ((parent-is "\\`\\(?:\\(?:boolea\\|compariso\\)n_operator\\)\\'") parent 0)}>
                    arg2) %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
        line_in_if(); %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
    elseif (condition2 || ... %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
            (condition3 && ... %  <{Matched rule: ((parent-is "\\`\\(?:\\(?:boolea\\|compariso\\)n_operator\\)\\'") parent 0)}>
             condition4)) %  <{Matched rule: ((parent-is "\\`\\(?:\\(?:boolea\\|compariso\\)n_operator\\)\\'") parent 0)}>
        disp('hello') %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
    elseif ... %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
        condition2 || ... %  <{Matched rule: ((parent-is "\\`\\(?:else\\(?:\\(?:if\\)?_clause\\)\\)\\'") parent 4)}>
        (condition3 && ... %  <{Matched rule: ((parent-is "\\`\\(?:\\(?:boolea\\|compariso\\)n_operator\\)\\'") parent 0)}>
         condition4) %  <{Matched rule: ((parent-is "\\`\\(?:\\(?:boolea\\|compariso\\)n_operator\\)\\'") parent 0)}>

        disp('hello') %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
    else ... %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>


    end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>

    if  a %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>


    end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>

    if    ... %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
        foo + ... %  <{Matched rule: ((parent-is "\\`\\(?:arguments_statement\\|e\\(?:numeration\\|vents\\)\\|f\\(?:or_statement\\|unction_definition\\)\\|if_statement\\|methods\\|properties\\|while_statement\\)\\'") parent 4)}>
        bar2 == 10 %  <{Matched rule: ((parent-is "\\`binary_operator\\'") parent 0)}>

    end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>

end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
