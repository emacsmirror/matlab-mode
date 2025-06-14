% -*- matlab-ts -*-
classdef Prop
    properties
        p0
        p1 double;
        p2 double = 0;
        p3 (1,1) double;
        p4 (1,1) double = 0;
        p5 (1,1) double {mustBePositive} = 0;
    end
end
