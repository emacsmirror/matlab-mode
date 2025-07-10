% -*- matlab-ts -*-
a = [1 2; 3 4];

% (t-utils-xr "C-a" "C-n" (re-search-forward "'") "C-b" (prin1 (matlab-ts-mode--show-paren-or-block)))
a';

% (t-utils-xr "C-a" "C-n" (re-search-forward "'") "C-b" (prin1 (matlab-ts-mode--show-paren-or-block)))
a.';

% (t-utils-xr (re-search-forward "<") "C-b" "C-b" (prin1 (matlab-ts-mode--show-paren-or-block)))
s1 = '<foo '' bar>';

% (t-utils-xr (re-search-forward ">") (prin1 (matlab-ts-mode--show-paren-or-block)))
s2 = '<foo '' bar>';

% (t-utils-xr (re-search-forward "<") "C-b" "C-b" (prin1 (matlab-ts-mode--show-paren-or-block)))
s3 = "<foo ' bar>";

% (t-utils-xr (re-search-forward ">") (prin1 (matlab-ts-mode--show-paren-or-block)))
s4 = "<foo ' bar>";

% (t-utils-xr (re-search-forward "<") "C-b" "C-b" (prin1 (matlab-ts-mode--show-paren-or-block)))
s5 = "<asdf

% (t-utils-xr (re-search-forward ">") (prin1 (matlab-ts-mode--show-paren-or-block)))
s6 = asdf>"
