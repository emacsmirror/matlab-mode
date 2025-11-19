% -*- matlab-ts -*-

% See https://github.com/acristoffers/tree-sitter-matlab/issues/136

% The comments after the end statements should be part of the statement node, i.e.
% comment3 shouldn't be under the properties node. Likewise comment5 shouldn't
% be part of the events node.

classdef parser_comments_after_end_statements_issue136 < handle

    % comment1
    properties
        foo;
        bar;
        % comment2
    end

    % comment3
    events
        goo;
        % comment4
    end

    % comment5
end

% comment6
