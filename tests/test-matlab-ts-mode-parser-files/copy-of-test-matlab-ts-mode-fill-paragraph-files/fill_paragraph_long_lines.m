% -*- matlab-ts -*-

function a = fill_paragraph_long_lines(b)
    if b
        % M-file or MEX-file are valid MATLAB Functions that must be mlocked and cleared so MATLAB will see the changes.
        a=1;
    else
        a=2;
    end

    a = a + foo;
end


function f = foo
% 1. a long comment comment comment comment comment comment comment comment comment comment comment comment comment comment comment comment comment

    f = 1;
    % 2. a long comment comment comment comment comment comment comment comment comment comment comment comment comment comment comment comment comment
    f = f + 1;

    f = f + goo(1,2,3,4);
end

function g = goo(diff, TOL, k, nmax)

    % Note following comment does not have a space after the '%' to make sure we have comment-start-skip correct. If we define (setq-local comment-start-skip "%\\s-+"), the fill will not work.

    while diff > TOL && k <= nmax %a long comment comment comment comment comment comment comment comment comment comment comment comment comment comment comment comment comment a long comment comment comment comment comment comment comment comment comment comment comment comment comment comment comment comment comment
        g = g + 1;
    end
end
