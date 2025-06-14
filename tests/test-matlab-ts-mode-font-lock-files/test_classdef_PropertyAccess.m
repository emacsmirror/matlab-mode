% -*- matlab-ts -*-
classdef test_classdef_PropertyAccess
   properties (GetAccess = {?ClassA, ?ClassB})
      Prop1
   end
   properties (Access = ?ClassC)
      Prop2
   end
end
