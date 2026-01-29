% -*- matlab-ts -*- %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
function indent_matrix %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
    a = [ ... %  <{Matched rule: ((parent-is "\\`function_definition\\'") parent matlab-ts-mode--set-function-indent-level-for-gp)}>
          1 ... %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
          + ... %  <{Matched rule: ((parent-is "\\`binary_operator\\'") parent 0)}>
          2 %  <{Matched rule: ((parent-is "\\`binary_operator\\'") parent 0)}>
        ]; %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>
    disp(a); %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>

    a = [    2 ... %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
             1 ... %  <{Matched rule: ((parent-is "\\`\\(?:function_output\\|row\\)\\'") parent 0)}>
        ]; %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>
    disp(a); %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>

    a = [1, 2; %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
         3, 4]; %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
    disp(a); %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>

    a = [ ... %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
          2 + [ 3 %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
                4, %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
                5 + [ ... %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
                      2 %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
                    ] %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>
              ] %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>
        ]; %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>
    disp(a); %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>

    a = [ ... %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
          1; ... %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
          2 ... %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
        ]; %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>
    disp(a); %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>

    long_variable_a = ... %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
        [ %  <{Matched rule: ((parent-is "\\`assignment\\'") parent 4)}>
          2, 123, 456 %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
          3,   2    7 %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
        ]; %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>
    disp(long_variable_a); %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>

end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
