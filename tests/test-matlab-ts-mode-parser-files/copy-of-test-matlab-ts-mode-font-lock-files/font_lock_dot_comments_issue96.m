% -*- matlab-ts -*-

% see https://github.com/acristoffers/tree-sitter-matlab/issues/96

function foo(in1)

    %.... block start ....

    if in1 < 2
        disp('a')
    elseif in1 >= 2
        disp('c');
    end

    %.... block end ....

end

