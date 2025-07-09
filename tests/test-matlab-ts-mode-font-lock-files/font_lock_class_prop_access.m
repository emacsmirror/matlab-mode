% -*- matlab-ts -*-
classdef font_lock_class_prop_access
   properties (GetAccess = {?ClassA, ?ClassB})
      Prop1
   end
   properties (Access = ?ClassC)
      Prop2
   end
end
