% -*- matlab-ts -*-

% For boolean expressions when in an if statement or assignement to a variable,
% we align them.

if obj.IsSomeLongFunction && ...
   ~strcmp(obj.property1, obj.property2) && ...
   someOtherCond
    disp('is true');
end

s.theThingEnabled = obj.IsSomeLongFunction && ...
                    ~strcmp(obj.property1, obj.property2) && ...
                    someOtherCond;
