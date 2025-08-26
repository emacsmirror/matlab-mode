% -*- matlab-ts -*-
% see https://github.com/acristoffers/tree-sitter-matlab/issues/99
for i=1:2
    try set(gui_hFigure, varargin{index}, varargin{index+1}), catch break, end
end
