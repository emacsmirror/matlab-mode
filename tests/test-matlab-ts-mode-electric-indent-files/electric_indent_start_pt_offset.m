% -*- matlab-ts -*-

% t-utils-test-indent: no-line-by-line-indent - line-by-line typing results in error nodes

function electric_indent_start_pt_offset()

% This exercises
%     (when (> start-pt-offset eol-col) ;; be robust too big a start-pt-offset
%       (setq start-pt-offset eol-col)))
% in matlab-ts-mode--ei-move-to-loc

foobar('Parameters', ...
       {'A', 'B', 'C', 'D', 'E', 'F'}, ...
       'Values', ...
                                    {1                      , 1, 1, 1, 1, 1});
