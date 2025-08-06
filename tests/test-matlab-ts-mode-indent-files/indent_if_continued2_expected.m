% -*- matlab-ts -*-

function indent_if_continued2

    if condition1 || ...
       condition2 || ...
       fcn_call(arg1, arg2)

        line_in_if();

    elseif condition1 + condition2 == ...
           2770000 || ...
           fcn_call(arg1, arg2)
        line_in_if();
    elseif (condition2 || ...
            (condition3 && ...
             condition4))
        disp('hello')
    elseif ...
        condition2 || ...
        (condition3 && ...
         condition4)

        disp('hello')
    else ...


    end

    if  a


    end

    if    ...
        foo + ...
        bar2 == 10

    end

end
