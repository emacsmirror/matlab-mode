% -*- matlab-ts -*-

% t-utils-test-indent: no-line-by-line-indent - line-by-line typing results in error nodes

% The assignment to x below has a TAB character starting the line and tab's in the line

if 1
    if 2
        x = a * b - c;
    end
end
