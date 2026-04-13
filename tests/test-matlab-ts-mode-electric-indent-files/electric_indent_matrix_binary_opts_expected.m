% -*- matlab-ts -*-

% t-utils-test-indent: no-line-by-line-indent - line-by-line typing results in error nodes
% preventing matrix column alignment.

m1 = [
       12345, a + b, a + b, a + b
           1, a + b, a + b, a + b
       % comment
           1, a - b, a - b, a - b
       c - d,     2,     3,     4
          10,    20,    30,    40
     ];

m2 = [12345, a + b, a + b, a + b
          1, a + b, a + b, a + b
       % comment
          1, a - b, a - b, a - b
      c - d,     2,     3,     4
         10,    20,    30,    40];
