% -*- matlab-ts -*-

% Unary +/- starts new entries: "a +b" => entries "a" and "+b".
% These are numeric-m-matrix because entries are simple tokens
% with optional unary prefix.

a = 1; b = 2; c = 3; d = 4;

m1 = [
       1 a +b
       1 a -b
     ];

% Unary only matrix should classify and align correctly.

m2 = [1 -1 -2 -3
      1 +1 +2 +3];

% Binary +/- expressions make a matrix non-numeric because the
% entry width changes after EI spacing.

m3 = [1, a + b
         3,     4];
