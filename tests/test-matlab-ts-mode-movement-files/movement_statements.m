% -*- matlab-ts -*-

% Case1: (t-utils-xr "C-n" "C-a" "C-f" (matlab-ts-mode-beginning-of-command) (matlab-ts-mode-end-of-command))
s = 'my string';

% Case2: (t-utils-xr "C-n" "C-a" "C-f" (matlab-ts-mode-beginning-of-command) (matlab-ts-mode-end-of-command))
m = [1, 2;
     3, 4];

% Case2: (t-utils-xr "C-n" "C-a" "C-f" (matlab-ts-mode-beginning-of-command) (matlab-ts-mode-end-of-command))
plot(1, 10)

% Case3: (t-utils-xr "C-n" "C-a" "C-f" (matlab-ts-mode-beginning-of-command) (matlab-ts-mode-end-of-command))
sprintf("%s", s)

% Case4: (t-utils-xr "C-n" "C-a" "C-f" (matlab-ts-mode-beginning-of-command) (matlab-ts-mode-end-of-command))
v=1:10;

% Case5: (t-utils-xr "C-n" "C-a" "C-f" (matlab-ts-mode-beginning-of-command) (matlab-ts-mode-end-of-command))
v(2:3)

% Case6: (t-utils-xr "C-n" "C-a" "C-f" (matlab-ts-mode-beginning-of-command) (matlab-ts-mode-end-of-command))
dependencies.internal.widget.foobar( ...
    this.UniqueName);

% Case7: (t-utils-xr "C-n" "C-a" "C-f" (matlab-ts-mode-beginning-of-command) (matlab-ts-mode-end-of-command))
this.foo3 = 1 + dependencies.internal.widget.someFunction( ...
    this.UniqueName2);

% Case8: (t-utils-xr "C-n" "C-a" "C-f" (matlab-ts-mode-beginning-of-command) (matlab-ts-mode-end-of-command))
this.foo3 = 1 + dependencies.internal.widget.someFunction2( ...
    a + foo(1 + 2 + ...
            3) + ...
    this.UniqueName3);

