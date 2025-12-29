% -*- matlab-ts -*-

classdef electric_indent_prop_dim
    properties
        foo1 ( 1, :) {mustBeNumeric, mustBeReal} = [0, 0, 0];
        foo2 ( 1, 3 ) {mustBeNumeric, mustBeReal} = [0, 0, 0];
        foo3 ( : , 3 ) {mustBeNumeric, mustBeReal} = [0, 0, 0];
        foo4 (:, : ) {mustBeNumeric, mustBeReal} = [0, 0, 0];
    end
end
