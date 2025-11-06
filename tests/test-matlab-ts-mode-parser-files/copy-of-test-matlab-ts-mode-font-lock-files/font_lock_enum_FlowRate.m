% -*- matlab-ts -*-
% See https://github.com/acristoffers/tree-sitter-matlab/issues/28
classdef font_lock_enum_FlowRate < int32
   enumeration
      Low    (10)
      Medium (FlowRate.Low*5)
      High   (FlowRate.Low*10)
   end
end
