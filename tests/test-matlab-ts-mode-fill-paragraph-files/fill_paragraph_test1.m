% -*- matlab-ts -*-
function b = fill_paragraph_test1(a)

    % Setup test to use a small fill column
    % (t-utils-xr (set-fill-column 60))
    %
    % Test of fill paragraph
    % (t-utils-xr "C-n" "C-n" "M-q")
    %
    % A very long comment. A very long comment. A very long comment. A very long comment. A very long comment.

    b = 1;
end
