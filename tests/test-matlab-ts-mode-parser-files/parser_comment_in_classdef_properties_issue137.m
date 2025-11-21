% -*- matlab-ts -*-

% See https://github.com/acristoffers/tree-sitter-matlab/issues/137

classdef parser_comment_in_classdef_properties_issue137
    properties
        Interlacing (1,1) logical = false;

        % comment1

        InterlaceIndex {mustBeVector, mustBeNumeric, mustBeInteger, mustBeNonnegative, mustBeLessThan(InterlaceIndex, 10)} = 0;
    end
end
