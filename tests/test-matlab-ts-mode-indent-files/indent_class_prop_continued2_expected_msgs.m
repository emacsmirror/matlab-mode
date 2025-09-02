% -*- matlab-ts -*- %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>

% t-utils-test-indent: no-line-by-line-indent - typing line by line doesn't work when there are no 'end' statements for classdef/properties %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>

classdef indent_class_prop_continued2 %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>

    properties (Constant) %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
        property1 = containers.Map(... %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
            { %  <{Matched rule: (matlab-ts--i-arg-namespace-fcn-prop-matcher matlab-ts--i-arg-namespace-fcn-prop-anchor 4)}>
              'one' %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
              'two' %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
            } ... %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>
            , ... %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
            { %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
              'foo' %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
              'bar' %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
            }); %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>

        property2 = someFcn(... %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
            { %  <{Matched rule: ((n-p-gp "\\`\\(?:)\\|arguments\\|line_continuation\\)\\'" "\\`function_call\\'" "\\`default_value\\'") great-grand-parent 4)}>
              'one' %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
              'two' %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
            } ... %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>
            , ... %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
            { %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
              'foo' %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
              'bar' %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent matlab-ts-mode--row-indent-level)}>
            }); %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>
    end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
