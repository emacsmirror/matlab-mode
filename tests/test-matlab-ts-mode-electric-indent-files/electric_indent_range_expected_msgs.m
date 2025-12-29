% -*- matlab-ts -*- %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

dataTbl = [dataTbl(:, 1 : varColNumbers(1) - 1) ... %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
           tableWithDateColumnOnly dataTbl(:, varColNumbers(1) + 1 : end)]; %  <{Matched rule: ((parent-is "\\`\\(?:function_output\\|row\\)\\'") parent 0)}>
