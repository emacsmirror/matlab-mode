% -*- matlab-ts -*-
function indent_comment_fcn
% This is the
% doc help comment

    nested();

    function nested

        % this is a nested comment for code
        disp('in nested');
        nested2;
    end

    function nested2
    % comment for nested2
        disp('in nested2');
        % comment after code
    end
end
