% -*- matlab-ts -*-
function imenu_f0
    f1
    f2
    x = f3;
    [a, b] = f4(2);
    a = F6;
    f7;
    foo123567890123567890123567890123567890123567890;
end

function...
        a = f1
    disp('in f1')
end

function f2
    disp('in f2')
end

function x = ...
        f3
    disp('in f3')
    function2 = 1;
    x = function2;
    f5;
    function f5
        disp('f5 in f3');
        f6;
        function F6
            disp('F6 in f5 in f3');
        end
    end
end

function [a, ...
          ...
          b ] ...
          = ...
          f4(c)
    function f5
        disp('in f5')
    end

    disp('in f4')
    f5;
    a = c;
    b = c;
end

function a = F6
    a = sqrt(2);
    disp('in f6');
end

function...
        f7
    disp('in f7')
end

function foo123567890123567890123567890123567890123567890
    disp('in foo123567890123567890123567890123567890123567890')
end
