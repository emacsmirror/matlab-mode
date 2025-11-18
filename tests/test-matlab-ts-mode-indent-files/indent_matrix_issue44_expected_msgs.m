% -*- matlab-ts -*- %  <{Matched rule: (maltab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

% https://github.com/acristoffers/tree-sitter-matlab/issues/44 %  <{Matched rule: (maltab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

foo = 1:2; %  <{Matched rule: (maltab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
bar = [ %  <{Matched rule: (maltab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
        foo(1), ... %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
        2 %  <{Matched rule: ((parent-is "\\`\\(?:function_output\\|row\\)\\'") parent 0)}>
      ] %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>
