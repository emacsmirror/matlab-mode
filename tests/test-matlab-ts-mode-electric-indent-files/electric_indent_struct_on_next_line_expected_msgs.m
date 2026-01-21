% -*- matlab-ts -*- %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

% For now we require struct be on the assignment line for alignment of the fields %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

% t-utils-test-indent: no-line-by-line-indent - line-by-line typing results in error nodes %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

myStruct.stateInfo(end + 1) = ... %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
    struct('foo', 1, ... %  <{Matched rule: ((parent-is "\\`assignment\\'") parent 4)}>
           'otherFieldTwo', 'something'); %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
