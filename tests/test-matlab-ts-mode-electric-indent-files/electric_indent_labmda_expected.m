% -*- matlab-ts -*-

l1 = @(x) ((ischar(x) || isstring(x)));

l2 = @(x) ((ischar(x) || isstring(x) || isnumeric(x)) && ...
           ~strcmpi(x, 'fubar'));

l3 = @(x) disp(x);
