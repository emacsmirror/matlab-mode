% -*- matlab-ts -*-

function b = thing_defun(a)

    % Case1: (t-utils-xr (beginning-of-defun))
    % Case2: (t-utils-xr "C-M-a")
    x = 1;

    % Case3: (t-utils-xr (end-of-defun) (end-of-defun) (end-of-defun) (end-of-defun))
    % Case4: (t-utils-xr "C-M-e" "C-M-e" "C-M-e" "C-M-e")

    b = nested1(a) + nested2(a);

    function out = nested1(in)
        out = x + helper1(in);
        % Case5: (t-utils-xr "C-M-a")
        % Case6: (t-utils-xr "C-M-a" "C-M-a")
        % Case7: (t-utils-xr "C-M-e" "C-M-e" "C-M-e" "C-M-e")
    end

    function out = nested2(in)

        % Case8: (t-utils-xr "C-M-a")
        out = 2 * x + in;
        % Case9: (t-utils-xr "C-M-a")
    end

    b = 2 * b;
end

function out = helper1(in)
    out = 1 + helper2(in);
end

function out = helper2(in)
    out = in *2;
    % Case10: (t-utils-xr "C-M-a" "C-M-a" "C-M-a" "C-M-a")
end
