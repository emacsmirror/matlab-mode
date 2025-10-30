% -*- matlab-ts -*- %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>
function indent_matrix %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>
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
         3, 4]; %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-anchor 0)}>
    disp(a); %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>

    a = [ ... %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
          2 + [ 3 %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
                4, %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-anchor 0)}>
                5 + [ ... %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-anchor 0)}>
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
