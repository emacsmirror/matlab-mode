% -*- matlab-ts -*-
function font_lock_error(foo)
    out = ['''(', newline];

    for x = 1:length(foo)
        c = foo(x);
        if regexp(c, '\(variable|slxFile\)"\)', 'once')
        else
            m = regexp(c, '^[ \t]*\(("[^"]+")[ \t]+\.[ \t]+".*\(mFile|pFile|mex\)"\)', tokens)
            if ~isempty(m)
                out = [out, ...
            end
        end
    end
end


