% -*- matlab-ts -*-

% t-utils-test-indent: no-line-by-line-indent - when typing matrix line-by-line, there are
% error nodes and thus the matrix alignment doesn't occur

% The following two matrices do not have blank lines between them. This validates that
% the indent-region of the matrix in the temporary buffer doesn't pickup invalid content.

if 1
        tp_taper_BorderVertices = [neg_BorderVertices;...
        topcover_taper_Face_BV];
    tp_taper_Polygons = [neg_Polygons;...
        topcover_taper_Face_poly{1} + max(max(neg_Polygons))];
end
