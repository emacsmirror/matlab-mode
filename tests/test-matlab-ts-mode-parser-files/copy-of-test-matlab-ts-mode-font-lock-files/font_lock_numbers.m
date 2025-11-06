% -*- matlab-ts -*-

a = 12.34;

b = 3;

c = 3e10;

d = [1:10;
     2:2:20];

e = d(2:end, end:end);

if e == 20
    disp('correct')
else
    disp('bug!')
end



