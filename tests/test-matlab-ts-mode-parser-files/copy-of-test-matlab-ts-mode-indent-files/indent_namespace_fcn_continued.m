% -*- matlab-ts -*-

function d=indent_namespace_fcn_continued(a, b)

    try
        d = [a;b];
    catch
        disp('[a;b] failed');
    end

    % Function arguments are aligned when the first argument follows the open parenthesis
    someNamespace1.subNamespace2.myFunction(a, ... % comment for param1
                                      b);    % comment for param2


    % Function arguments are indented and aligned when the first argument is on the next line:
    someNamespace1.subNamespace2.myFunction( ...
         a, ... % comment for param1
           b);    % comment for param2

end % this function has an end
