% -*- matlab-ts -*- %  <{Matched rule: (maltab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

% See https://github.com/acristoffers/tree-sitter-matlab/issues/125 %  <{Matched rule: (maltab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

% There is whitespace after both "someCmd  " statements below %  <{Matched rule: (maltab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

switch in1 %  <{Matched rule: (maltab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
  otherwise %  <{Matched rule: ((node-is "\\`\\(?:\\(?:ca\\|otherwi\\)se_clause\\)\\'") parent 2)}>
    if in1 > 1 %  <{Matched rule: ((parent-is "\\`\\(?:case_clause\\|otherwise_clause\\|switch_statement\\)\\'") parent 2)}>
        disp('> 1'); %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
    else %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
        someCmd %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
    end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>


switch a %  <{Matched rule: (maltab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
  otherwise %  <{Matched rule: ((node-is "\\`\\(?:\\(?:ca\\|otherwi\\)se_clause\\)\\'") parent 2)}>
    someCmd %  <{Matched rule: ((parent-is "\\`\\(?:case_clause\\|otherwise_clause\\|switch_statement\\)\\'") parent 2)}>
    if a > 1 %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
        disp('>1') %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
    end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
