% -*- matlab-ts -*- %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

% t-utils-test-indent: no-line-by-line-indent - when we type line-by-line, we don't see later lines %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

for wIndex = 1 : length(foo.blah) %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
    fooBarGooToo = [something, ... %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
                    [otherThing(wIndex) : theEndIndex(wIndex) - 1, ... %  <{Matched rule: ((parent-is "\\`\\(?:function_output\\|row\\)\\'") parent 0)}>
                     otherThing(wIndex) + 1 : theEndIndex(wIndex); ... %  <{Matched rule: ((parent-is "\\`\\(?:function_output\\|row\\)\\'") parent 0)}>
                     otherThing(wIndex) + 1 : theEndIndex(wIndex), ... %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
                     otherThing(wIndex) : theEndIndex(wIndex) - 1]]; %#ok<AGROW> %  <{Matched rule: ((parent-is "\\`\\(?:function_output\\|row\\)\\'") parent 0)}>
    P0 = [P0, foo.blah{wIndex}.foobarGooOrig(:, 1 : end - 1)]; %#ok<AGROW> %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
    P1 = [P1, foo.blah{wIndex}.foobarGooOrig(:, 2 : end)];     %#ok<AGROW> %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>


for wIndex = 1 : length(foo.blah) %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
    fooBarGooToo = [something, ... %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
                    {otherThing(wIndex) : theEndIndex(wIndex) - 1, ... %  <{Matched rule: ((parent-is "\\`\\(?:function_output\\|row\\)\\'") parent 0)}>
                     otherThing(wIndex) + 1 : theEndIndex(wIndex); ... %  <{Matched rule: ((parent-is "\\`\\(?:function_output\\|row\\)\\'") parent 0)}>
                     otherThing(wIndex) + 1 : theEndIndex(wIndex), ... %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
                     otherThing(wIndex) : theEndIndex(wIndex) - 1}]; %#ok<AGROW> %  <{Matched rule: ((parent-is "\\`\\(?:function_output\\|row\\)\\'") parent 0)}>
    P0 = [P0, foo.blah{wIndex}.foobarGooOrig(:, 1 : end - 1)]; %#ok<AGROW> %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
    P1 = [P1, foo.blah{wIndex}.foobarGooOrig(:, 2 : end)];     %#ok<AGROW> %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
