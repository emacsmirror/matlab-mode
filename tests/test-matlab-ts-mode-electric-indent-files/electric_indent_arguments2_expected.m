% -*- matlab-ts -*-

% t-utils-test-indent: no-line-by-line-indent - when typing line-by-line we can't align the
% arguments because we don't have them all.

function electric_indent_arguments2( ...
    firstToAnalyze, argTwo, generateFoobar, NameValues)
    arguments
        firstToAnalyze string {mustBeScalarOrEmpty}
        argTwo { ...
                 Aero.internal.validation.mustBeEmptyStringOrStateSpace ...
               } = ''
        generateFoobar            (1,1) matlab.lang.OnOffSwitchState = "off"
        NameValues.SecondDocument (1,1) string {mustBeMember(NameValues.SecondDocument, ["Document1", "Document2"])} = "Document1"
        NameValues.Level          (1,1) string {mustBeMember(NameValues.Level, ...
                                                             ["Lowest", "All", "1", "2", "3"])} = "Lowest"
    end

    disp(firstToAnalyze);
    disp(argTwo);
    disp(generateFoobar);
    disp(NameValues);
end
