% -*- matlab-ts -*-

% t-utils-test-indent: no-line-by-line-indent - when typing line-by-line we can't align the
% trailing comments because we don't see later lines

x   = [1, 2, 3]; % comment 1 (aligned)
xyz = 2;         % comment 2 (aligned)


if x
    y = 1
end % end if x
% other comment
z = y;

if x
    y = 1
end % end if x
x = 1 % this shouldn't be aligned with prior

if z % some comment
    y = [1, 2] % some comment
end

foo1 blah % comment (aligned)
c = 1;    % comment (aligned)

;     % comment (aligned)
cfooo % comment (aligned)

if a && ...
   b % comment
    z = [1, 2, 2, 4] % comment (not aligned)
end
