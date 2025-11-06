% -*- matlab-ts -*-

classdef (abstract) font_lock_keywords_as_others2 < handle & matlab.mixin.SetGetExactNames
    properties (Access='public')
        AP = [];
        AB = 'charvec with space';
        AC = "string with space and ( ";
        AD = fun_call(1,2);
        AE (1,:) double {mustBePositive} = 1;
    end

    properties (AbortSet=true, NonCopyable=true)
        AF (1,1) char {mustBeMember(AF, {'High','Medium','Low'})} = 'Low';
        AG (1,1) matlab.lang.OnOffSwitchState = 'on';
    end

    events
        Event1
        Event2
    end

    methods
        function obj = mclass()
            obj.AB = obj.AP(1:end);
            unusedvar = 1;
            disp('charvect with if and for words [ in it');
            notify(obj,'Event1',...
                       'indent test');
            notify(obj, 'Event1', 'indent test');
            while obj.AB
                disp("while loop going on here (");
            end
            error('function mclass in charvec }');
        end
    end
    methods (Access='public')
        function meth(obj)
            if obj.AP
                disp('display the word end here');
            else
                try
                catch
                end
            end
        end
    end
    methods (Abstract, Hidden=true)
        result = abs_func(a,b)
        result = other_abs_fun(a,b)
    end
    methods
        function end_separate_line(~)
        end
        function end_same_line(~), end
        function after_end_same_line(~), end
    end
    methods
        function properties(~)
        end
        function methods(~)
        end
        function events(~)
        end
        function arguments(~)
        end
        function enumeration(~)
        end
        function usestuff(obj)
            obj.properties();
            obj.methods();
            obj.events();
            obj.arguments();
            obj.enumeration();
        end
    end
end
