% -*- matlab-ts -*- %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

clear s %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
s.foo.events ... %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
    .x = 1; %  <{Matched rule: ((n-p-gp nil nil "\\`assignment\\'") grand-parent 4)}>
s.foo.enumeration ... %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
    .x = 2; %  <{Matched rule: ((n-p-gp nil nil "\\`assignment\\'") grand-parent 4)}>
s.foo.methods ... %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
    .x = 3; %  <{Matched rule: ((n-p-gp nil nil "\\`assignment\\'") grand-parent 4)}>
s.foo.arguments ... %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
    .x = 4; %  <{Matched rule: ((n-p-gp nil nil "\\`assignment\\'") grand-parent 4)}>

disp(s.foo.events); %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
disp(s.foo.enumeration); %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
disp(s.foo.methods); %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
disp(s.foo.arguments); %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
