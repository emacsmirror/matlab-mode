% -*- matlab-ts -*-
function [a, b] = test_comment_heading
% help for test_comment_heading
%
% % isn't a section heading in the doc comment
%

    %%% not a comment heading

    %%foo not a comment heading, must have at least one space

    %% comment heading 1
    % text here
    %
    %% not a comment heading
    %
    % some text  %% not a comment heading

    a = 1; %% not a comment heading

    %% comment heading 2

    b = 1;
end
