% Take from myBar in
% https://www.mathworks.com/help/matlab/matlab_prog/validate-name-value-arguments.html
%
% See: https://github.com/acristoffers/tree-sitter-matlab/issues/58

function font_lock_fcn_arguments_issue58(x,y,propArgs)
    arguments
        x (:,:) double
        y (:,:) double
        propArgs.?matlab.graphics.chart.primitive.Bar
        propArgs.FaceColor {mustBeMember(propArgs.FaceColor,{'red','blue'})} = "blue"
    end
    propertyCell = namedargs2cell(propArgs);
    bar(x,y,propertyCell{:})
end
