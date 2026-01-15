% -*- matlab-ts -*- %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

% t-utils-test-indent: no-line-by-line-indent - line-by-line typing results in error nodes %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

try %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
    foo() %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
catch err %comment %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
