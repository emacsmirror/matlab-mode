% -*- matlab-ts -*- %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>

myVariable = struct( ... %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>
    'Short',  {1}, ... %  <{Matched rule: (matlab-ts-mode--i-assign-cont-matcher matlab-ts-mode--i-assign-cont-anchor matlab-ts-mode--i-assign-cont-offset)}>
    'Long',   {100} ... %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
    ); %  <{Matched rule: (matlab-ts-mode--i-assign-cont-matcher matlab-ts-mode--i-assign-cont-anchor matlab-ts-mode--i-assign-cont-offset)}>

myVariable2 = some.namespace.fcn( ... %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>
    'Short',  {1}, ... %  <{Matched rule: (matlab-ts-mode--i-assign-cont-matcher matlab-ts-mode--i-assign-cont-anchor matlab-ts-mode--i-assign-cont-offset)}>
    'Long',   {100} ... %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
    ); %  <{Matched rule: (matlab-ts-mode--i-assign-cont-matcher matlab-ts-mode--i-assign-cont-anchor matlab-ts-mode--i-assign-cont-offset)}>
