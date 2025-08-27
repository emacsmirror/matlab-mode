% -*- matlab-ts -*-

function a=indent_odd_else_if(b)
    if b >= 1
        a=1;
    else if b <= -1
             a = -1;
         else
             a=0;
         end
         a = a + ok_else_if(b);
    end
end

function a=ok_else_if(b)
% test_m_odd_else_if_unindented "if statement" is equivalent to:
    if b >= 1
        a=1;
    else
        if b <= -1
            a = -1;
        else
            a=0;
        end
    end
end
