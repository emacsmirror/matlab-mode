% -*- matlab-ts -*-

% We get a parse tree with a \n node after the function. From
%   https://github.com/acristoffers/tree-sitter-matlab/issues/136
%
% The \n nodes need to be ignored by Emacs.
%
% From acristoffers (in github issue)
%
%   Ok, there is a newline, so it matches. Why one matches and the other doesn't is because a
%   function is parsed by the internal parser and it emits the newline and then the function token,
%   whereas the identifier in a = 1 is emitted by the external scanner, which skips the newline. I
%   won't try to fix it because it's most probably going to break other seemingly unrelated things
%   and because the presence of '\n' is not really a problem here. I would say that most of the time
%   those newlines can just be ignored as the nodes give enough information to figure things out,
%   but that will of course depend on what the thing consuming the tree is doing (and how).

function b = parser_newline_after_function_issue136(a)
end
