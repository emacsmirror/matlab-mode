% -*- matlab-ts -*-

% t-utils-test-indent: no-line-by-line-indent - when typing line-by-line we can't align the
% properties because we don't have them all.

classdef electric_indent_class_properties
    properties
        foo(1,3)
        foobar    (1, 1)
        p1  ( 1, 1)
        x  {  mustBeReal}
        y;
    end
end
