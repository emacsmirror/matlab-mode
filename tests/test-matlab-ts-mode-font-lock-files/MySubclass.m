% -*- matlab-ts -*-
classdef MySubclass < ParentClass
% help comment
    properties
        MyProperty double = 0; % Public property with a default value
    end

    methods
        function obj = MyClass(initialValue)
            % Constructor method
            obj.MyProperty = initialValue;
        end

        function newValue = getMyProperty(obj)
            % Getter method
            newValue = obj.MyProperty;
        end

        function obj = setMyProperty(obj, newValue)
            % Setter method
            obj.MyProperty = newValue;
        end
    end
end
