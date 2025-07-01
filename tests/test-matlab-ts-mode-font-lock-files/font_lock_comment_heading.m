% -*- matlab-ts -*-
function [a, b] = test_comment_heading
% help for test_comment_heading
%
% % isn't a section heading in the doc comment
%

    % text before heading
    %% comment heading 1
    %
    % some comment
    %
    %% comment heading 2 within one comment
    %
    % some text

    a = 1;

    %% comment heading 3

    b = 1;
end
