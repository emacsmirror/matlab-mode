% -*- matlab-ts -*-
classdef test_classdef_events < handle
    events
       StateChange
    end
    methods
       function triggerEvent(obj)
          notify(obj,'StateChange')
       end
    end
end
