% -*- matlab-ts -*-

% Filling code with ellipsis and strings causes problems for prog-fill-reindent-defun
% so we created matlab-ts-mode-prog-fill-reindent-defun to test that M-q on these works.

function [a, b, f2, f3, f4] = fill_paragraph_strings_and_ellipsis(c)

    % c1: (t-utils-xr "C-n" "C-a" "M-q")
    a = c + ...
                2;

    % c2: (t-utils-xr "C-n" "C-a" "M-q")
    a = a  + 3; % foo bar    goo

    b = foo1;

    f2 = foo2;
    f3 = foo3;
    f4 = foo4;
end

function b = foo1

    % c3: (t-utils-xr (re-search-forward "a long string") "M-q")
           b = "a long string a long string a long string a long string a long string a long string a long string a long string a long string a long string a long string a long string a long string a long string ";
end

function foobar = foo2

    % c4: (t-utils-xr "C-n" "C-a" "M-q")
    foobar = struct(...
        'field1', 1, ...
            'field1', 2);
end

function foobar = foo3

    % c5: (t-utils-xr "C-n" "C-n" "C-a" "M-q")
    foobar = struct(...
        'field1', 1, ...
            'field1', 2);
end

function foobar = foo4

    % c6: (t-utils-xr "C-n" "C-n" "C-n" "C-a" "M-q")
    foobar = struct(...
        'field1', 1, ...
            'field1', 2);
end

% c7: (t-utils-xr (t-utils-xr-print-code (point-min) (point-max)))
