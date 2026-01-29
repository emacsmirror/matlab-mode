% -*- matlab-ts -*-

% t-utils-test-indent: no-line-by-line-indent - line-by-line typing results in error nodes

function electric_indent_arguments_constraints(input1, params)
    arguments
        input11 (:,:)

        params.foo1       = lines(1)
        params.foobar     = 'auto'
        params.fooBarZoo  = 'auto'
        params.otherThing ...
            {mustBeNumeric, mustBeReal, mustBeFinite, mustBeNonnegative, ...
             mustBeNonsparse, mustBeLessThanOrEqual(params.otherThing, 1)} = 1
    end
end
