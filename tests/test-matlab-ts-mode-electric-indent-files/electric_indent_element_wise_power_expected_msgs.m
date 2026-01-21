% -*- matlab-ts -*- %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

% .^ operator is element-wise power %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

% MATLAB tree-sitter identifies 10. as a number, thus 10.^5 and 10 .^ 5 have differnt node types. %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

a = 5; %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
b = 10; %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

x = 10 .^ 5 %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
y = 10 .^ a; %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
z = a.^b; %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
