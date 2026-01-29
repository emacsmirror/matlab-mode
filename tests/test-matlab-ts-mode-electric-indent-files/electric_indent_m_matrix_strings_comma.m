% -*- matlab-ts -*-

% t-utils-test-indent: no-line-by-line-indent - line-by-line typing results in error nodes

s1 = ['ab'     'cd'];

s2 = ["ab"     "cd"];

abc = 'foo';

s3 = ['  ' abc ' with properties:' 10 10 ...
            '                       type: {' 10];
