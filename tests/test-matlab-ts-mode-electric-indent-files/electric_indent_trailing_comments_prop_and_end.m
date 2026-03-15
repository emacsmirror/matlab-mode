% -*- matlab-ts -*-

% t-utils-test-indent: no-line-by-line-indent - when typing line-by-line we can't align

classdef electric_indent_trailing_comments_prop_and_end
    properties
        property1; % list of properties affected by interlacing for current channel instance
    end %properties Dependent protected

    properties (Access = private, Transient)   % comment1
        foobar         = false;               % comment2
        foobarTime     = 0;                   % comment3
        foobarGooTooSupported = opc.ua.Node.empty;   % comment4
        foobarToken    (1,1) logical = false; % comment5
    end

end
