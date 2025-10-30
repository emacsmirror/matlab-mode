% -*- matlab-ts -*- %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>

someStr = "[" + ... %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>
          "[[""A"", 1], [""B"", 2]]," + ... %  <{Matched rule: ((parent-is "\\`binary_operator\\'") parent 0)}>
          "[[""C"", 3]]," + ... %  <{Matched rule: ((parent-is "\\`binary_operator\\'") parent 0)}>
          "[[""D"", 4], [""E"", 5], [""F"", 6]]" + ... %  <{Matched rule: ((parent-is "\\`binary_operator\\'") parent 0)}>
          "]"; %  <{Matched rule: ((parent-is "\\`binary_operator\\'") parent 0)}>
