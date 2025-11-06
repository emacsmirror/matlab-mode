% -*- matlab-ts -*-

% see https://github.com/acristoffers/tree-sitter-matlab/issues/102

s.function = {1, 2; ...
              3, 4};

a = [s.function{:,2}]

s.function{:,2}

c = s.function{1,2};

d = [s.function{1,2}];

% ./+namespace1/+namespace2/foo.m
%   function a = foo
%       a = 2;
%   end

e = [namespace1.namespace2...
     .foo]

