% -*- matlab-ts -*- %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>
var_a = my_function(1, 2, 3); %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>

var_b = my_function(1, ... %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>
                    2, ... %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
                    3); %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>

my_struct.var_c = my_function( ... %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>
    1, ... %  <{Matched rule: (matlab-ts-mode--i-assign-cont-matcher matlab-ts-mode--i-assign-cont-anchor matlab-ts-mode--i-assign-cont-offset)}>
    2, ... %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
    3); %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>

my_other_function(1, ... %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>
                  2, ... %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
                  3); %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>

% some extra spaces after a function call %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>
my_other_function  ( ... %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>
    1, ... %  <{Matched rule: ((node-is "\\`arguments\\'") parent 4)}>
    2, ... %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
    3); %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
