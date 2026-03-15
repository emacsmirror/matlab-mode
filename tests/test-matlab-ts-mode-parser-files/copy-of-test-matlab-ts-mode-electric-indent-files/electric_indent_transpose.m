% -*- matlab-ts -*-

% .^ binds tighly (no spaces) when the left and right are "atoms", otherwise there's spaces:

a0 = [1, 2] .^ 2;
a1 = T .^ 2;
a2 = T' .^ 2;
