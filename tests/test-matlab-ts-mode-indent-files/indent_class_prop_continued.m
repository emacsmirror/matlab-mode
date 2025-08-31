% -*- matlab-ts -*-

% t-utils-test-indent: no-line-by-line-indent - need to improve continued properties in
%                                               matlab-ts-mode--i-next-line-matcher

classdef indent_class_prop_continued
    properties
        ListArrayHeight = struct( ...
            'Short',  {1}, ...
            'Medium', {50}, ...
            'Long',   {100} ...
            );
    end
end
