% -*- matlab-ts -*-
function ... % foo
    imenu_g0
    g1
    g2
    x = g3;
    [a, b] = g4(2);
end

function...
        a = ... % foo
        g1
    disp('in g1')
end

function ...
...
...%foo
        g2
    disp('in g2')
end

function x = ... % [a,b] =
        g3
    disp('in g3')
    function2 = 1;
    x = function2;
end

function [ ...
    a, ... % comment for a
    b  ... % comment for b
         ] ... 
         = ...
         g4(c)
    function g5
        disp('in g5')
    end

    disp('in g4')
    g5;
    a = c;
    b = c;
end
