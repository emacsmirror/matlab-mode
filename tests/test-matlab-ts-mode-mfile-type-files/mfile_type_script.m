% -*- matlab-ts -*-

% (t-utils-xr (print (format "mfile-type: %S" (matlab-ts-mode--mfile-type))))

a = 1;
b = myfcn(a);

function y=myfcn(x)
    y = 2 * x;
end  
