% -*- matlab-ts -*-
% Test forward/backward page keybindings:
% Case1: (t-utils-xr (goto-char (point-min)) "C-x ]" "C-x ]" "C-x [" "C-x [")

% Do similar using the underlying functions:
% Case2: (t-utils-xr (goto-char (point-min)) (forward-page) (forward-page) (backward-page) (backward-page))

%% Section 1

x = 1;
y = 2;

%% Section 2

z = x+y;

