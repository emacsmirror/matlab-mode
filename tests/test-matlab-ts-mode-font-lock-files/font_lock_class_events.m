% -*- matlab-ts -*-
classdef font_lock_class_events < handle
    events
       StateChange
    end
    methods
       function triggerEvent(obj)
          notify(obj,'StateChange')
       end
    end
end
