% -*- matlab-ts -*-
classdef MultiplePropBlocks
    properties (SetAccess = private, GetAccess = {?OtherClass})
        Property1
        Property2
    end
    properties (Abstract)
        Property3
    end
end
