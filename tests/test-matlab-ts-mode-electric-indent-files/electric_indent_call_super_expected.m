% -*- matlab-ts -*-

classdef electric_indent_call_super < myUtils.super
    methods
        function obj = electric_indent_call_super
            obj@myUtils.super;
            disp('in SubClass')
        end
    end
end
