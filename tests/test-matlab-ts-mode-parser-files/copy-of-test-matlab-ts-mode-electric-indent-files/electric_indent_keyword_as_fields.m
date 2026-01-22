% -*- matlab-ts -*-

clear s
s.foo.events...
    .x = 1;
s.foo.enumeration...
    .x = 2;
s.foo.methods...
    .x = 3;
s.foo.arguments ...
    .x = 4;

disp(s.foo.events);
disp(s.foo.enumeration);
disp(s.foo.methods);
disp(s.foo.arguments);
