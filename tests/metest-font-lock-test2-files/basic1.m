classdef basic1
    properties (Access = public)
        % aa1, bb1 cc1
        aaa = 0
        % aa2, bb2
        MyPublicData (1,:) double {mustBePositive} = [1 1 1]
    end
end
