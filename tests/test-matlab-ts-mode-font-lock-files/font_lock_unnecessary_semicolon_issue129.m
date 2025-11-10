% -*- matlab-ts -*-

% See: https://github.com/acristoffers/tree-sitter-matlab/issues/129


function font_lock_unnecessary_semicolon_issue129
    ;
    %comment
    unnecessary_semicolon
end

function unnecessary_semicolon
; %#ok undocumented
    disp('here')
end

 
