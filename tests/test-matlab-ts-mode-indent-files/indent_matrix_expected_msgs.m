% -*- matlab-ts -*- %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>
function indent_matrix %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>
    a = [ ... %  <{Matched rule: ((parent-is "\\`function_definition\\'") parent matlab-ts-mode--set-function-indent-level-for-gp)}>
          1 ... %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
          + ... %  <{Matched rule: ((parent-is "\\`\\(?:function_output\\|row\\)\\'") parent 0)}>
          2 %  <{Matched rule: (matlab-ts-mode--i-cont-matcher parent matlab-ts-mode--i-cont-offset)}>
        ]; %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>
    disp(a); %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>

    a = [    2 ... %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
             1 ... %  <{Matched rule: ((parent-is "\\`\\(?:function_output\\|row\\)\\'") parent 0)}>
        ]; %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>
    disp(a); %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>

    a = [1, 2; %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
         3, 4]; %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
    disp(a); %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>

    a = [ ... %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
          2 + [ 3 %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
                4, %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
                5 + [ ... %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
                      2 %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
                    ] %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>
              ] %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>
        ]; %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>
    disp(a); %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>

    a = [ ... %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
          1; ... %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
          2 ... %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
        ]; %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>
    disp(a); %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>

    long_variable_a = ... %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
        [ %  <{Matched rule: ((parent-is "\\`assignment\\'") parent 4)}>
          2, 123, 456 %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
          3,   2    7 %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
        ]; %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>
    disp(long_variable_a); %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>

end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
