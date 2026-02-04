% -*- matlab-ts -*-

% https://github.com/acristoffers/tree-sitter-matlab/issues/155

a = true;
b = false;
myNamespace.myFcn(a && (~b));

% Where we have file ./+myNamespace/myFcn.m which contains
%    function myFcn(in1)
%        disp(in1)
%    end

assert((~expr1), 'message');

a = 1; b = 1; c = 1;
assert((~a || b) && c, 'message');

assert(~(~isempty(aConstraint) && ~isempty(aCondition)))
