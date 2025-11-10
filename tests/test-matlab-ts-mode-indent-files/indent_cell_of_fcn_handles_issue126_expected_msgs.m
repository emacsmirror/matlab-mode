% -*- matlab-ts -*- %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>

% See https://github.com/acristoffers/tree-sitter-matlab/issues/126 %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>

v = { ... %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>
      @(p) f1(p), ... %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
      @(p) f2(p) ... %  <{Matched rule: ((parent-is "\\`\\(?:function_output\\|row\\)\\'") parent 0)}>
      @(p) f3(p) ... %  <{Matched rule: ((parent-is "\\`\\(?:function_output\\|row\\)\\'") parent 0)}>
    } %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>
