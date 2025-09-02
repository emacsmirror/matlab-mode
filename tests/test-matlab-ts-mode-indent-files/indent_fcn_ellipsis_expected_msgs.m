% -*- matlab-ts -*- %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>
function ... %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>
    [    ... %  <{Matched rule: ((parent-is "\\`function_definition\\'") parent matlab-ts-mode--set-function-indent-level-for-gp)}>
     a,  ... comment about a %  <{Matched rule: ((parent-is "\\`\\(?:function_arguments\\|multioutput_variable\\)\\'") parent 1)}>
     b   ... comment about b %  <{Matched rule: ((parent-is "\\`\\(?:function_arguments\\|multioutput_variable\\)\\'") parent 1)}>
    ]    ... %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>
    = indent_ellipsis ... %  <{Matched rule: ((parent-is "\\`\\(?:function_output\\|row\\)\\'") parent 0)}>
    (   ... %  <{Matched rule: ((parent-is "\\`function_definition\\'") parent matlab-ts-mode--set-function-indent-level-for-gp)}>
     c, ...  comment about c %  <{Matched rule: ((parent-is "\\`\\(?:function_arguments\\|multioutput_variable\\)\\'") parent 1)}>
     d, ...  comment about d %  <{Matched rule: ((parent-is "\\`\\(?:function_arguments\\|multioutput_variable\\)\\'") parent 1)}>
     e  ...  comment about e %  <{Matched rule: ((parent-is "\\`\\(?:function_arguments\\|multioutput_variable\\)\\'") parent 1)}>
    ) %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>

    a =     ... %  <{Matched rule: ((parent-is "\\`function_definition\\'") parent matlab-ts-mode--set-function-indent-level-for-gp)}>
        1 + ... %  <{Matched rule: ((parent-is "\\`assignment\\'") parent 4)}>
        (   ... %  <{Matched rule: ((parent-is "\\`binary_operator\\'") parent 0)}>
         c ... %  <{Matched rule: ((parent-is "\\`parenthesis\\'") parent 1)}>
         * ... %  <{Matched rule: ((parent-is "\\`binary_operator\\'") parent 0)}>
         d ... %  <{Matched rule: ((parent-is "\\`binary_operator\\'") parent 0)}>
        ); %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>

    b = [ ... %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
          1 + ... %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
          2 + ... %  <{Matched rule: ((parent-is "\\`binary_operator\\'") parent 0)}>
          3   ... %  <{Matched rule: ((parent-is "\\`binary_operator\\'") parent 0)}>
        ]; %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>

end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
