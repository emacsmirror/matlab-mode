% -*- matlab-ts -*-

function font_lock_fcn_arguments2_issue57(a,b)
    arguments
        a (1,:) double {mustBeReal}
        b (1,:) double {mustBeReal, myUtils.mustBeSameSize(a, b)}
    end

    disp(a)
    disp(b)
end
