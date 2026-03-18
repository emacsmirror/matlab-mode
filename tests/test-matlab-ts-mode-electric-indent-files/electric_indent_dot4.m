% -*- matlab-ts -*-

% t-utils-test-indent: no-line-by-line-indent

classdef electric_indent_dot4 < foo.bar.Base.AlgorithmGenerator.Base
    methods
        function this = electric_indent_dot4
            this@foo.bar.Base. ...
                     AlgorithmGenerator.Base(10);
        end
    end
end
