% -*- matlab-ts -*-

function out = electric_indent_keyword_as_var(in1, arguments)
    arguments
        in1 {mustBeTextScalar}
        arguments cell {mustBeText} = {}
    end

    % following both returns the first entry of arguments, then the arguments, and a {1}!
    out = {in1, arguments{1}, arguments, {1}};
end
