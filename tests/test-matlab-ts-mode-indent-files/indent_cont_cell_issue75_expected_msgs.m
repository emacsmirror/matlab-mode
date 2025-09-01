% -*- matlab-ts -*- %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>

c1 ... %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>
    = ... %  <{Matched rule: ((parent-is "\\`assignment\\'") parent 4)}>
    { ... %  <{Matched rule: ((parent-is "\\`assignment\\'") parent 4)}>
      'foo', ... %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
      'bar' ... %  <{Matched rule: ((parent-is "\\`\\(?:function_output\\|row\\)\\'") parent 0)}>
      { ... %  <{Matched rule: ((parent-is "\\`\\(?:function_output\\|row\\)\\'") parent 0)}>
        'a'; ... %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
        'b'  ... %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
      }   ... %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>
      ''  ... %  <{Matched rule: ((parent-is "\\`\\(?:function_output\\|row\\)\\'") parent 0)}>
    } %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>
