% -*- matlab-ts -*-

function b = defun_type(a)

    % (t-utils-xr (beginning-of-defun))
    x = 1;

    % (t-utils-xr (end-of-defun) (end-of-defun) (end-of-defun) (end-of-defun))

    b = nested1(a) + nested2(a);

    function out = nested1(in)
        out = x + helper1(in);
        % (t-utils-xr (beginning-of-defun))
        % (t-utils-xr (beginning-of-defun) (beginning-of-defun))
        % (t-utils-xr (end-of-defun) (end-of-defun) (end-of-defun) (end-of-defun))
    end

    function out = nested2(in)

        % (t-utils-xr (beginning-of-defun))
        out = 2 * x + in;
        % (t-utils-xr (beginning-of-defun))
    end

end

function out = helper1(in)
    out = 1 + helper2(in);
end

function out = helper2(in)
    out = in *2;
    % (t-utils-xr (beginning-of-defun) (beginning-of-defun) (beginning-of-defun))
end
