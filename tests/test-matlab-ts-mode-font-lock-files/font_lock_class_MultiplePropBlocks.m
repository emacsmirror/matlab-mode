% -*- matlab-ts -*-
classdef font_lock_class_MultiplePropBlocks
    properties (SetAccess = private, GetAccess = {?OtherClass})
        Property1
        Property2
    end
    properties (Abstract)
        Property3
    end
end
