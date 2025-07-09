% -*- matlab-ts -*-
function font_lock_variable

    global bar bar2
    persistent foo1;

    foo2 = 'a'

    total = 0;
    for idx = 1:10
        total = total + idx;
    end

    [a, b] = fcn1();
end

function [a, b] = fcn1
    a = 1;
    b = 2;
end

