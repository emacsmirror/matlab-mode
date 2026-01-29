% -*- matlab-ts -*-

% The MATLAB tree-sitter doesn't require "...", thus validate we handle missing "..."

% t-utils-test-indent: no-line-by-line-indent - line-by-line typing results in error nodes

myStruct.stateInfo(end + 1) = struct(

    'fieldOne'     , someThing || otherThing,
    'otherFieldTwo', 'something');
