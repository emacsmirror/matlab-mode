% -*- matlab-ts -*-

% For now we require struct be on the assignment line for alignment of the fields

% t-utils-test-indent: no-line-by-line-indent - line-by-line typing results in error nodes

myStruct.stateInfo(end + 1) = ...
    struct('foo', 1, ...
           'otherFieldTwo', 'something');
