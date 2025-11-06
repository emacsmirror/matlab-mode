% -*- matlab-ts -*-

% t-utils-test-indent: no-line-by-line-indent - nested functions require an end

function b = indent_nested(a)

    x = 1;

    b = nested1(a);
    
    function d = nested1(c)
    % help comment for
    % nested1

        % comment about next line, y =

        y = 2;

        d = nested2(c) + x;

        function f = nested2(e)
        % help comment for nested2

            % comment about next line, f =
            f = y + e * 3;
        end
    end

    function nested3
    end

end
