% -*- matlab-ts -*-

dataTbl = [dataTbl(:, 1:varColNumbers(1)-1)...
           tableWithDateColumnOnly dataTbl(:, varColNumbers(1) + 1 : end)];
