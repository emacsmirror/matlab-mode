% -*- matlab-ts -*-

% See https://github.com/acristoffers/tree-sitter-matlab/issues/125

% There is whitespace after both "someCmd  " statements below

switch in1
  otherwise
    if in1 > 1
        disp('> 1');
    else
        someCmd  
    end
end


switch a
  otherwise
    someCmd  
    if a > 1
        disp('>1')
    end
end
