% -*- matlab-ts -*-

% Note, there should be whitespace on line after the enumeration!
% See: https://github.com/acristoffers/tree-sitter-matlab/issues/48
classdef font_lock_enum_with_blank_non_empty_line
    enumeration
  
        red % parse error when the prior line is a blank line containing spaces or tabs
    end
end
