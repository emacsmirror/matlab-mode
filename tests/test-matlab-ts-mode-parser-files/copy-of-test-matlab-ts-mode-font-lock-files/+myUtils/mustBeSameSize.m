% -*- matlab-ts -*-

function mustBeSameSize(in1, in2, in1Name, in2Name)
    if ~isequal(size(in1), size(in2))
        error("%s and %s must be same size", in1Name, in2Name);
    end
end
