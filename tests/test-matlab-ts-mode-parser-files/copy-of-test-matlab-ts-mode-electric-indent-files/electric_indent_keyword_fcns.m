% -*- matlab-ts -*-

% t-utils-test-indent: no-line-by-line-indent - line-by-line typing results in error nodes

classdef electric_indent_keyword_fcns < handle

    properties
        normalprop = 1;
    end

    properties (Access= 'public')
        % See if we can create properties using keywords
        %properties = 1;
        %methods = 1;
        %events = 1;
        arguments
        prop = 1;
    end

    events (Access= 'private')
        %properties
        %events
        %methods
        arguments
    end

    methods

        function simple_method(obj)
            arguments
                obj
            end

            disp(obj.normalprop);
        end

        function obj = blocks(arguments, events, properties, methods, enumeration)

            arguments
                arguments
                events
                properties
                methods
                enumeration
            end

            obj.prop = arguments;
            obj.prop = events;
            obj.prop = properties;
            obj.prop = methods;
            obj.prop = enumeration;
        end

        function properties(~)
        end

        function methods(~)
        end

        function events(~)
        end

        function arguments (~)
        end

        function enumeration(~)
        end

        function usestuff(obj)
        % Try using the methods of this object
            obj.properties();
            obj.methods();
            obj.events();
            obj.arguments ();
            obj.enumeration();
        end

    end

end
