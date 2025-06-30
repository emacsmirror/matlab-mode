% -*- matlab-ts -*-
function b = comments_basic(a)

    % Test M-; to comment then uncomment following two statements.
    % (t-utils-xr "C-a" "C-n" "C-SPC" "C-n" "C-n" "M-;" "M-;" (deactivate-mark))
    b = a * 2;
    b = b * 3;

    % Test M-; to add comment to end of statement, then delete it.
    % (t-utils-xr "C-a" "C-n" "M-;" (insert "foo") "C-a" "M-;" (re-search-backward ";") "C-f" "C-k")
    b = b * 4;
end
