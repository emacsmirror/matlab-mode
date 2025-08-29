% -*- matlab-ts -*-

% t-utils-test-indent: no-line-by-line-indent - doc comment indent requires classdef end

classdef (Hidden) indent_comment_classdef_doc
% comment
% comment

% Copyright blah

    methods (Static)
        function foo
        end
    end
end
