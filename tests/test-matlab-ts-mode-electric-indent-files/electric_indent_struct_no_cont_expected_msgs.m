% -*- matlab-ts -*- %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

% The MATLAB tree-sitter doesn't require "...", thus validate we handle missing "..." %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

% t-utils-test-indent: no-line-by-line-indent - line-by-line typing results in error nodes %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

myStruct.stateInfo(end + 1) = struct( %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

    'fieldOne',      someThing || otherThing, %  <{Matched rule: ((n-p-gp nil nil "\\`assignment\\'") grand-parent 4)}>
    'otherFieldTwo', 'something'); %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
