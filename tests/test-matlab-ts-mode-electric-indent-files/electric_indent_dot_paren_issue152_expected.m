% -*- matlab-ts -*-

% See: https://github.com/acristoffers/tree-sitter-matlab/issues/152

obj = myClass;

a = 1;

obj.(~a);

obj(~a);

obj{~a};

%  classdef myClass
%      methods
%          % For
%          %   >> obj = myClass
%          % subsref lets you overload operators:
%          %   obj.(expr) - "dot paren index" (often called dynamic field reference) takes the value of the
%          %                 expression inside parens and treats that as a dot-index.
%          %   obj(expr)  - "paren index"
%          %   obj{expr}  - "brace index"
%          function varargout = subsref(obj, S)
%              disp('in subref');
%              disp(S);
%              varargout{1} = 0;
%              varargout{2} = 0;
%          end
%      end
%  end
