% -*- matlab-ts -*-
function imenu_myFunc1
    a = myFunc2(2);
    b = myFunc3(a);
    myFunc4(b);
end

function out = myFunc2(in)
    out = in * 2;
end

function [out] = myFunc3(in)
    out = in * 3;
end

function myFunc4(in)
    disp(num2str(in))
end
