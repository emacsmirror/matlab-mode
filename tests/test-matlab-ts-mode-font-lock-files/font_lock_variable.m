% -*- matlab-ts -*-
function font_lock_variable

    % ((assignment left: (identifier) @matlab-ts-mode-variable-override-builtin-face
    %              (:pred matlab-ts-mode--is-variable-overriding-builtin
    %                     @matlab-ts-mode-variable-override-builtin-face)))
    disp = 10;

    % (assignment left: (identifier) @font-lock-variable-name-face)
    foo2 = 'a'

    % (multioutput_variable (identifier) @matlab-ts-mode-variable-override-builtin-face
    %              (:pred matlab-ts-mode--is-variable-overriding-builtin
    %                     @matlab-ts-mode-variable-override-builtin-face))
    [disp, plot] = fcn1();

    % (multioutput_variable (identifier) @font-lock-variable-name-face)
    [a, b] = fcn1();

    % (global_operator (identifier) @matlab-ts-mode-variable-override-builtin-face
    %               (:pred matlab-ts-mode--is-variable-overriding-builtin
    %                      @matlab-ts-mode-variable-override-builtin-face))
    global disp plot

    % (global_operator (identifier) @font-lock-variable-name-face)
    global bar1 bar2

    % (persistent_operator (identifier) @matlab-ts-mode-variable-override-builtin-face
    %              (:pred matlab-ts-mode--is-variable-overriding-builtin
    %                     @matlab-ts-mode-variable-override-builtin-face))
    persistent plot;

    % (persistent_operator (identifier) @font-lock-variable-name-face)
    persistent foo1;

    total = 0;

    % (for_statement (iterator (identifier) @matlab-ts-mode-variable-override-builtin-face
    %                          (:pred matlab-ts-mode--is-variable-overriding-builtin
    %                                 @matlab-ts-mode-variable-override-builtin-face)))
    for plot = 1:10
        total = total + plot;
    end


    % (for_statement (iterator (identifier) @font-lock-variable-name-face))
    for idx = 1:10
        total = total + idx;
    end


end

function [a, b] = fcn1
    a = 1;
    b = 2;
end

