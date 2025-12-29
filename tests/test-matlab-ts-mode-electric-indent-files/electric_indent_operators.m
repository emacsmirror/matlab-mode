% -*- matlab-ts -*-

if ~isempty(foo(1).SrcBlock) && bar(1).SrcBlock ~= -1
    disp('here')
end

D = B * r^2;
x = r.^2;

s =  D';
t =           D.';

foo(p,'folder',   @(x) validateattributes(x,{'char','string'},{'nonempty'}));

mc   =?matlab.mixin.SetGet;

P(hTmp) = P0(1)*theta.^(-g0/(L(1)*R));

a2 = (1 + 2) ^ (3 + 4);

a3 = (1 + 2) .^ (3 + 4);

a = 2;
b = 3;

m1 = 2^3 * 4;
m2 = a ^  4 * 4;
m3 = a ^  b * 4;
m4 = 2 ^ (3 * 4);

m5 = 2.^3 * 4;
m6 = a .^  4 * 4;
m7 = a .^  b * 4;
m8 = 2 .^ (3 * 4);

function myBar(x,y,propArgs)
    arguments
        x (:,:) double
        y (:,:) double
        propArgs .? matlab.graphics.chart.primitive.Bar
    end
    propertyCell = namedargs2cell(propArgs);
    bar(x,y,propertyCell{:})
end
