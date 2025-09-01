% -*- matlab-ts -*- %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>

data = struct(... %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>
    'f', f, ... %  <{Matched rule: (matlab-ts-mode--i-assign-cont-matcher matlab-ts-mode--i-assign-cont-anchor matlab-ts-mode--i-assign-cont-offset)}>
    'm', struct('id', 1, 'other', 'two'), ... %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
    't', 'xyz', ... %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
    'input', ['[' text ']']); %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>

data = struct('f', f, ... %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>
              'm', struct('id', 1, 'other', 'two'), ... %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
              't', 'xyz', ... %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
              'input', ['[' text ']']); %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
