% -*- matlab-ts -*-

% t-utils-test-indent: no-line-by-line-indent - line-by-line typing results in error nodes

% Notic on the left there is a matrix. We align the one on the right

a.b([c, d], :) = [foo1(overlapIdx, :) + in(oneStart : overlap, :); ...
in( overlap + oneStart : end, :)];

a.b([c, d], :) = [foo1(overlapIdx, :) + in(oneStart : overlap, :);
in(overlap + oneStart : end, :)];

a.b([c, d], :) = [foo1(overlapIdx, :) + in(oneStart : overlap, :);
% comment
in(overlap + oneStart : end, :)];

a.b([c, d], :) = [foo1(overlapIdx, :) + in(oneStart : overlap, :); ...
% comment
in(overlap + oneStart : end, :)];
