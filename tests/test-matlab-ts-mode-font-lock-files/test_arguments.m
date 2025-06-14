% -*- matlab-ts -*-
function test_arguments(in1, properties, options)
    arguments
       in1 (1,:) {mustBeNumeric}
       properties double
       options.LineStyle (1,1) string = "-" 
       options.LineWidth (1,1) {mustBeNumeric} = 1
    end
end
