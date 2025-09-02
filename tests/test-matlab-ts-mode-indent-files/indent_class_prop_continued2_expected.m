% -*- matlab-ts -*-

% t-utils-test-indent: no-line-by-line-indent - typing line by line doesn't work when there are no 'end' statements for classdef/properties

classdef indent_class_prop_continued2

    properties (Constant)
        property1 = containers.Map(...
            {
              'one'
              'two'
            } ...
            , ...
            {
              'foo'
              'bar'
            });

        property2 = someFcn(...
            {
              'one'
              'two'
            } ...
            , ...
            {
              'foo'
              'bar'
            });
    end
end
