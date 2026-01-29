% -*- matlab-ts -*-

function [m, s] = electric_indent_keyword_fcns2(x)
    arguments, x (:,:) {mustBeNumeric}, end
    [m1, s1] = DoStat(x);
    [m2, s2] = DoStat2(x);
    [m3, s3] = DoStat3(x);
    m        = m1 + m2 - m3;
    s        = s1 + s2 - s3;
end

function [m, s] = DoStat(arguments)
    arguments, arguments (:,:) {mustBeNumeric}, end
    m = mean(arguments, 'all');
    s = std(arguments, 1, 'all');
end

function [m, s] = DoStat2(arguments)
    arguments,
        arguments (:,:) {mustBeNumeric},
    end

    m = mean(arguments, 'all');
    s = std(arguments, 1, 'all');
end

function [m, s] = DoStat3(arguments)
    arguments, arguments (:,:) {mustBeNumeric},
    end
    a.arguments = arguments;
    m           = mean(a.arguments, 'all');
    s           = std(a.arguments, 1, 'all');
    s           = foo3(s);
end

function methods = foo3(properties)
    methods = arguments(properties);
end

function events = arguments(arguments)
    arguments, arguments (:,:) {mustBeNumeric}, end
    enumeration ...
        = arguments;
    if enumeration > 0
        arguments = -enumeration;
    end
    events ...
        = arguments + 1;
end
