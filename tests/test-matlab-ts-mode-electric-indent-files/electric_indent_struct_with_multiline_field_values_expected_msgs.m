% -*- matlab-ts -*- %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

% t-utils-test-indent: no-line-by-line-indent - line-by-line typing results in error nodes %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

foobar = struct('type'   , {'one', 'two', ... %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
                            'three', 'four'}, ... %  <{Matched rule: ((parent-is "\\`\\(?:function_output\\|row\\)\\'") parent 0)}>
                'fooport', {1, 2, 3, 4, 4, 4, 4, 4, 4, 1, 2}, ... %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
                't'      , 1, ... %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
                'x'      , struct('a', 1, ... %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
                                  'foo', 10)); %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
