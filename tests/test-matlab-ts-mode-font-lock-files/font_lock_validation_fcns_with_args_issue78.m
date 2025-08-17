% -*- matlab-ts -*-

% see https://github.com/acristoffers/tree-sitter-matlab/issues/78

function font_lock_validation_fcns_with_args_issue78(alpha, beta)
    arguments
        alpha (1, :) double {mustBeReal}
        beta  (1, :) double {mustBeReal, myUtils.mustBeSameSize(beta, alpha, "beta", "alpha")}
    end
end
