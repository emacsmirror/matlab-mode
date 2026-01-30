% -*- matlab-ts -*-

r = myFunction(Name1 = 3, Name2 = 7)

function result = myFunction(NameValueArgs)
    arguments
        NameValueArgs.Name1
        NameValueArgs.Name2
    end

    % Function code
    result = NameValueArgs.Name1 * NameValueArgs.Name2;
end
