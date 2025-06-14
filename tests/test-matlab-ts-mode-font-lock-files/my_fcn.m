% -*- mode: matlab-ts -*-
function [out1, out2, out3] = my_fcn(in1, in2, in3)
% help comment
% help comment line 2

    % code comment
    if in1 > double(5) %#some-pragma
       out1 = sin(10) + in * [2];
    else
       out1 = in1 * 3 + 2;
    end

    qStr = "asdf asdf' asdf '";
    out2 = foo2(qStr);

    sStr = 'asdf''asdf"';
    out3 = foo3(sStr);
end

function out1 = foo2(in1)
    out1 = in1;
end

function [out1] = foo3(in1)
    out1 = in1;
end
