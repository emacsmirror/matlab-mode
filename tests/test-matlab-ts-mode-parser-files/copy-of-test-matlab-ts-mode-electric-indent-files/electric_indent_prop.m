% -*- matlab-ts -*-

function s= electric_indent_prop(  s  , NameValues)

    arguments
        s (1, 1) double
        NameValues.   Rename(1, 1) matlab.lang.OnOffSwitchState =     "on"
    end

    s = foo(s,[  ],NameValues.Rename);
end
