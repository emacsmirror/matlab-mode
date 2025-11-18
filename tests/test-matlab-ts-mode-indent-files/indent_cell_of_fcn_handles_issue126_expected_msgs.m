% -*- matlab-ts -*- %  <{Matched rule: (maltab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

% See https://github.com/acristoffers/tree-sitter-matlab/issues/126 %  <{Matched rule: (maltab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

v = { ... %  <{Matched rule: (maltab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
      @(p) f1(p), ... %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
      @(p) f2(p) ... %  <{Matched rule: ((parent-is "\\`\\(?:function_output\\|row\\)\\'") parent 0)}>
      @(p) f3(p) ... %  <{Matched rule: ((parent-is "\\`\\(?:function_output\\|row\\)\\'") parent 0)}>
    } %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>
