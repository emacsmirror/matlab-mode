% -*- matlab-ts -*-

s.field1 = 1 : 10;

% Adding a space between the close and open parens, )(, would change the meaning.
c1 = {s.('field1')(3)};

c2 = {1 : 10};

c3 = {c2{1}(2)};
