% -*- matlab-ts -*-
function b = ts_defun_name_fcn(a)

    b = 0;
    if a > 1
        if a > 2
            disp('1 2')
            switch a
              case 2
                disp('case 2')
                b = fcn1(a);
            end
        end
    end
end

function b = fcn1(a)
    b = 5 * fcn2(a);

    function d = fcn2(c)
        d = c * 2;
    end
end
