% -*- matlab-ts -*-

function a=indent_lots_of_end_words
    a=bar();
end % end foo_end_comment

function B=bar

    if foo
        A = 1;
    end % end of iff

    B = A(1:end); %#ok

    if foo
        C = "this is the end of the line";

    end;  D = "string end string";

    E = [ D C];

    if bar

        A = E;

    end; B = A(1:end);

    E = B;

    if baz

        A = C;

    end; B = [ 1 2 ... is this the end?
               3 4 ];

    if foo

        A = E;

    end ... the other end

    B = [ B A ];

    % Multi-ends

    if foo %#ok
        if bar %#ok
            if baz

                A = B;

            else

            end; end; end % comment end thing

    B = goo(A);

end

function result = goo(b)

    result=1;
    % see xyz function
    % foobar
    for x = 1:length(b)
        result = x + result;
    end
end
