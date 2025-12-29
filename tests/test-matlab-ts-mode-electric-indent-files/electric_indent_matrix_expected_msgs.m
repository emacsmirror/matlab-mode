% -*- matlab-ts -*- %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

a0 = [1; 1]; %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

a1 = [[1]; [7]]; %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

a2 = [[1, 3, (4 + 4) * 2]; [5, 6, 7]]; %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

b = [[1, 3, 4]; [5, 6, 7]] %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

c = [1, 2]; %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

d = [0 0]; %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

e = 0 %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

e + e %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

[-e +e] %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

f = [e + e -e +e] %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

g = -e %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

h = [1 2; ... %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
     3 4]; %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-anchor 0)}>

c2 = {['freq' '%'] num2str(2)}; %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

c3 = {a b [c '%'] f(1)}; %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

x = [ %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
      1, 2; %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
      3, 4; %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
    ]; %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>

x2 = [ %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
       1, 2; %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
       3, 4; %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
     ]; %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>


d1 = dictionary([nativeProperties(:, 1); propertyStateArray(:, 1)], ... %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
                [nativeProperties(:, 2); propertyStateArray(:, 2)]); %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>

d2 = dictionary([nativeProperties(:, 1); propertyStateArray(:, 1)], ... %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
                [nativeProperties(:, 2); propertyStateArray(:, 2)]); %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>

m3 = uint8([ones(20, 1); 2 * ones(8, 1); ones(8, 1); 2 * ones(8, 1); ones(7, 1)]); %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

foo123(t0, :) = tan(th(t0) / 2) .* [sx(t0)' sy(t0)' sz(t0)']; %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

c3 = {{[17.50 0] [17.50 0]} {[120 0] [120 20]}}; %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>


c4{1} = [1 2; 3 4]; %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
v4 = [c4{1}(1, 1), c4{1}(1, 1)]; %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
