% -*- matlab-ts -*-

function b = thing_defun(a)

    % (t-utils-xr (beginning-of-defun))
    % (t-utils-xr "C-M-a")
    x = 1;

    % (t-utils-xr (end-of-defun) (end-of-defun) (end-of-defun) (end-of-defun))
    % (t-utils-xr "C-M-e" "C-M-e" "C-M-e" "C-M-e")

    b = nested1(a) + nested2(a);

    function out = nested1(in)
        out = x + helper1(in);
        % (t-utils-xr "C-M-a")
        % (t-utils-xr "C-M-a" "C-M-a")
        % (t-utils-xr "C-M-e" "C-M-e" "C-M-e" "C-M-e")
    end

    function out = nested2(in)

        % (t-utils-xr "C-M-a")
        out = 2 * x + in;
        % (t-utils-xr "C-M-a")
    end

    b = 2 * b;
end

function out = helper1(in)
    out = 1 + helper2(in);
end

function out = helper2(in)
    out = in *2;
    % (t-utils-xr "C-M-a" "C-M-a" "C-M-a" "C-M-a")
end
