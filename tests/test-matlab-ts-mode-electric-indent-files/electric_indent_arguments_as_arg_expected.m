% -*- matlab-ts -*-

% t-utils-test-indent: no-line-by-line-indent - when typing line-by-line we can't align

function [success, stdout, stderr] = electric_indent_arguments_as_arg(obj, command, arguments, inDir, buildLogger)
    arguments
        obj
        command     (1,:) char
        arguments   (1,:) cell {mustBeText}
        inDir       (1,:) char
        buildLogger (1,1) utils.BuildLogger
    end

    [success, stdout, stderr] = obj.exec(command, arguments, inDir, buildLogger);
end
