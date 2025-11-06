% -*- matlab-ts -*-

% See: https://github.com/acristoffers/tree-sitter-matlab/issues/78

% >> font_lock_validation_fcns_with_args2_issue78([1 2 3+2i; 0 2 3; 0 0 1])
% Error using font_lock_validation_fcns_with_args2_issue78 (line 13)
%  font_lock_validation_fcns_with_args2_issue78([1 2 3+2i; 0 2 3; 0 0 1])
%                                               ^^^^^^^^^^^^^^^^^^^^^^^^
% Invalid argument at position 1. Input must be a real-valued, upper triangular matrix.

function font_lock_validation_fcns_with_args2_issue78(mat)
    arguments
        mat (:, :) double {myUtils.nested.namespace.mustBeRealUpperTriangular}
    end
end
