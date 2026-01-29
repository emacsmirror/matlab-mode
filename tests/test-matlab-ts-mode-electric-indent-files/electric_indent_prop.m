% -*- matlab-ts -*-

% t-utils-test-indent: no-line-by-line-indent - when typing line-by-line we can't align the
% arguments because we don't have them all.

function s= electric_indent_prop(  s  , NameValues)

    arguments
        s (1, 1) double
        NameValues.   Rename(1, 1) matlab.lang.OnOffSwitchState =     "on"
    end

    s = foo(s,[  ],NameValues.Rename);
end
