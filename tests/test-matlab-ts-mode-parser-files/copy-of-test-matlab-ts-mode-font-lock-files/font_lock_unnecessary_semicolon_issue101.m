% -*- matlab-ts -*-

% see https://github.com/acristoffers/tree-sitter-matlab/issues/101

function font_lock_unnecessary_semicolon_issue101(in1)
    ;
    if in1
        disp('in1')
    end
end
