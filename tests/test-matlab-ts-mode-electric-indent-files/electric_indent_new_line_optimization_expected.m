% -*- matlab-ts -*-

% t-utils-test-indent: no-line-by-line-indent

% In matlab-ts-mode--ei-get-new-line we have an optimization where we
% defer matlab--eilb-setup and matlab-ts-mode--ei-insert-indent-level-spaces.
% This test point exercises that.

% Already indented code:

a1 = [1, 2];

if sum(a1) > 0
    if x > 1
        disp('sum > 0 && x > 1');
    end
end

% Unindented code:

a2 = [1, 2];

if sum(a2) > 0
    if x > 1
        disp('sum > 0 && x > 1');
    end
end
