% -*- matlab-ts -*-
% See https://github.com/acristoffers/tree-sitter-matlab/issues/29
classdef attributeDemo
   methods (Access = protected)
      function out = method1(obj,inputArg)
         ...
      end
   end
   methods (Access = private)
      function out = method2(obj,inputArg)
          ...
      end
   end
end
