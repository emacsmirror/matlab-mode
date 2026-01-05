% -*- matlab-ts -*-

% Exercise matrix indentation with varying indent levels

% t-utils-test-indent: no-line-by-line-indent - when typing matrix line-by-line, there are
% error nodes and thus the matrix alignment doesn't occur

if foo1
    m1 = [ 23, 2
          233, 4];
end


if foo1
    if foo2
        m2 = [ 23,    2
              233,    4
                1, 3344];
    end
end
