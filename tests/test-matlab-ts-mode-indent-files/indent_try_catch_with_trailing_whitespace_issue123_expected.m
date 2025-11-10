% -*- matlab-ts -*-

% https://github.com/acristoffers/tree-sitter-matlab/issues/123

% Below the "catch ME  " should have trailing whitespace to exercise tree-sitter

classdef indent_try_catch_with_trailing_whitespace_issue123
    properties
        m
    end

    methods
        function obj = foo(m)
        end

        function rtn = fcn1(obj, val)
            try
                rtn = obj.m * [1, 2; 3 4];
            catch ME
                if val == 1
                    disp('here1');
                else
                    disp('here2');
                end
                rtn = [];
            end
        end

    end
end
