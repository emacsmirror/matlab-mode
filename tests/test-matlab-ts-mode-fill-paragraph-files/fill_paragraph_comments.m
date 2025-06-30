% -*- matlab-ts -*-
function b = fill_paragraph_basic(a)
%
% Setup test to use a small fill column
% (t-utils-xr (set-fill-column 60))
%
% Test of fill paragraph
% (t-utils-xr "C-n" "C-n" "M-q")
%
% A very long comment. A very long comment. A very long comment. A very long comment. A very long comment. A very long comment. A very long comment. A very long comment. A very long comment. A  very long comment. A very long comment. A very long comment.
%
% Test of fill paragraph on a numbered list
% (t-utils-xr "C-n" "C-n" "M-q")
% 
% 1. Item one that is long. Item one that is long. Item one that is long. Item one that is long. Item one that is long. Item one that is long.
%    And has multiple lines
%
% Test of fill paragraph on a numbered list
% (t-utils-xr "C-n" "C-n" "M-q")
%
% 2. Item two that is long. Item two that is long. Item two that is long. Item two that is long. Item two that is long. Item two that is long.
%
% Test of fill paragraph on a bullet item.
% (t-utils-xr "C-n" "C-n" "M-q")
%
% - Item three that is long. Item three that is long. Item three that is long. Item three that is long. Item three that is long. Item three that is long. Item three that is long.
%

% Test of fill paragraph on a regular comment
% (t-utils-xr "C-n" "C-n" "M-q")
%
% foo bar foo barfoo bar foo barfoo bar foo bar foo barfoo bar foo barfoo bar foo bar foo barfoo bar foo barfoo bar
%

    if a > 1
        % Test of fill paragraph on a regular comment after a statement from column 1
        % (t-utils-xr "C-n" "M-q")
        b = a * 2; % A long comment at the end of a statement.  A long comment at the end of a statement.  A long comment at the end of a statement.  A long comment at the end of a statement.
        b = x * 1;
    else
        % Test of fill paragraph on a regular comment after a statement when in comment
        % (t-utils-xr "C-n" "C-a" "M-;" "M-q")
        b = c * d; % A long comment at the end of a statement.  A long comment at the end of a statement.  A long comment at the end of a statement.  A long comment at the end of a statement.
    end
end
