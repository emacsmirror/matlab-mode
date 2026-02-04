% -*- matlab-ts -*-

% Pretend xcp.a2l.Project.Warnings builtin is a "rich" object - try different index operations all
% should show as a "built-in" font because xcp.a2l.Project.Warnings is in
% matlab-ts-mode--builtins.el

xcp.a2l.Project.Warnings{1}
xcp.a2l.Project.Warnings{1}{2}
xcp.a2l.Project.Warnings{1}{2}(3)

