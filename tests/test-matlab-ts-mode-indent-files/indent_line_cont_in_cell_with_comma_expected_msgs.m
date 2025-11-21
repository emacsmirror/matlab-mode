% -*- matlab-ts -*- %  <{Matched rule: (maltab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

% t-utils-test-indent: no-line-by-line-indent - "} ..." is not handled correctly %  <{Matched rule: (maltab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

someFcn(... %  <{Matched rule: (maltab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
    'arg1', ... %  <{Matched rule: ((node-is "\\`arguments\\'") parent 4)}>
    ... comment1 %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
    '-foo1', ... %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
    { %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
      ... comment 2 %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
      {'foo2', ... %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
       '-foo3', ... %  <{Matched rule: ((parent-is "\\`\\(?:function_output\\|row\\)\\'") parent 0)}>
       @(f)cmd([a '/' b 'c'], 'foo4', 'on'), ... %  <{Matched rule: ((parent-is "\\`\\(?:function_output\\|row\\)\\'") parent 0)}>
       '-foo5', {'x'}, ... %  <{Matched rule: ((parent-is "\\`\\(?:function_output\\|row\\)\\'") parent 0)}>
      } ... %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>
      ... %  <{Matched rule: ((parent-is "\\`\\(?:function_output\\|row\\)\\'") parent 0)}>
      , ... comment %  <{Matched rule: ((parent-is "\\`\\(?:function_output\\|row\\)\\'") parent 0)}>
      {'foo', ... %  <{Matched rule: ((parent-is "\\`\\(?:function_output\\|row\\)\\'") parent 0)}>
       'bar'} ... %  <{Matched rule: ((parent-is "\\`\\(?:function_output\\|row\\)\\'") parent 0)}>
    }); %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>

function someFcn(varargin) %  <{Matched rule: (maltab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
    disp(varargin); %  <{Matched rule: ((parent-is "\\`function_definition\\'") parent matlab-ts-mode--set-function-indent-level-for-gp)}>
end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
