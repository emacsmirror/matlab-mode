% -*- matlab-ts -*-

% See: https://github.com/acristoffers/tree-sitter-matlab/issues/93


function font_lock_empty_endless_fun
foo1;
foo2;

function foo1
% empty foo1

function foo2
disp('in foo2')
