% -*- matlab-ts -*-

classdef indent_classdef_super_continued2 ...
    < otherThing & ...
      otherThing2 & ...
      handle
    properties (Dependent, SetAccess = private)
        TopicName
    end
end
