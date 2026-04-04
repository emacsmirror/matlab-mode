% -*- matlab-ts -*-

% t-utils-test-indent: no-line-by-line-indent - results in error nodes preventing electric indent

m1 = [  1, 200;
      300,   4]

m2 = [  1, 200;
      300,   4];

m3 = [  1, 200;
      300,   4],

m4 = [  1, 200;
      300,   4] % comment

m5 = [  1, 200;
      300,   4],,;;;;


m6 = [  1, 200;
      300,   4] ... continue

m7 = [  1, 200;
      300,   4],,;;; %comment

m8 = [  1, 200;
      300,   4],,;;; ... continue

not1 = [1, 200;
        300, 4]; x = 1;

not2 = [1, 200;
        300, 4],, y = 1;

not3 = [1, 200;
        300, 4],,;;; z = 1;
