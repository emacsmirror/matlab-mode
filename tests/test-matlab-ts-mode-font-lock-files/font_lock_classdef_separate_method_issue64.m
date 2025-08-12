% -*- matlab-ts -*-

% See: https://github.com/acristoffers/tree-sitter-matlab/issues/64

classdef font_lock_classdef_separate_method_issue64
    properties
        p1
    end

    methods
        function obj=foo(in)
            obj.p1 = in;
        end

        % Declare f2. Implementation in ./@foo/f2.m
        %                  function out1 = f2(obj, in1)
        %                      out1 = obj.p1 * in1;
        %                  end

        out1 = f2(obj, in1);
    end

end



