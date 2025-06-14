% -*- matlab-ts -*-
% See https://github.com/acristoffers/tree-sitter-matlab/issues/28
classdef FlowRate < int32
   enumeration
      Low    (10)
      Medium (FlowRate.Low*5)
      High   (FlowRate.Low*10)
   end
end
