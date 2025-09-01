% -*- matlab-ts -*- %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>
function indent_switch(in) %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>


    switch in %  <{Matched rule: ((parent-is "\\`function_definition\\'") parent matlab-ts-mode--set-function-indent-level-for-gp)}>


      case 10 %  <{Matched rule: ((node-is "\\`\\(?:\\(?:ca\\|otherwi\\)se_clause\\)\\'") parent 2)}>

        disp('one') %  <{Matched rule: ((parent-is "\\`\\(?:case_clause\\|otherwise_clause\\|switch_statement\\)\\'") parent 2)}>


        disp('two'); %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>


        a = 1 %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>

      otherwise %  <{Matched rule: ((node-is "\\`\\(?:\\(?:ca\\|otherwi\\)se_clause\\)\\'") parent 2)}>

        disp('foo') %  <{Matched rule: ((parent-is "\\`\\(?:case_clause\\|otherwise_clause\\|switch_statement\\)\\'") parent 2)}>


    end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>

end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
