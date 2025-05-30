function out = lspExample(a, b)
% LSPEXAMPLE - Language Server Protocol example

    f1 = func1(a);
    f2 = func2(b);

    f3 = func3(a+b);
    
    out = f1 + f2 + f3;
end

function out = func1(in)
    out = in + 1;
end

function out = func2(in)
    out = in + 2;
end

