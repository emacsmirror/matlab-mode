% -*- matlab-ts -*-

% t-utils-test-indent: no-line-by-line-indent - can't align due errors when matrix is incomplete

m1 = [a+b a+ b a-b, a- b
        exp(a)+b a+ b a-b a- b
      foo(a,1)+foo2(b) a+ b a-b a- b];
