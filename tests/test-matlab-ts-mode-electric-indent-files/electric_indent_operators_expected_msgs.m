% -*- matlab-ts -*- %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

if ~isempty(foo(1).SrcBlock) && bar(1).SrcBlock ~= -1 %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
    disp('here') %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>

D = B * r^2; %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
x = r.^2; %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

s = D'; %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
t = D.'; %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

foo(p, 'folder', @(x) validateattributes(x, {'char', 'string'}, {'nonempty'})); %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

mc = ?matlab.mixin.SetGet; %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

P(hTmp) = P0(1) * theta .^ (-g0 / (L(1) * R)); %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

a2 = (1 + 2) ^ (3 + 4); %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

a3 = (1 + 2) .^ (3 + 4); %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

% comment %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

a = 2; %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
b = 3; %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

% comment %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

m1 = 2^3 * 4; %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
m2 = a^4 * 4; %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
m3 = a^b * 4; %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
m4 = 2 ^ (3 * 4); %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

m5 = 2.^3 * 4; %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
m6 = a.^4 * 4; %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
m7 = a.^b * 4; %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
m8 = 2 .^ (3 * 4); %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

function myBar(x, y, propArgs) %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
    arguments %  <{Matched rule: ((parent-is "\\`function_definition\\'") parent matlab-ts-mode--set-function-indent-level-for-gp)}>
        x (:,:) double %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
        y (:,:) double %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
        propArgs.?matlab.graphics.chart.primitive.Bar %  <{Matched rule: ((parent-is "\\`\\(?:arguments_statement\\|e\\(?:numeration\\|vents\\)\\|f\\(?:or_statement\\|unction_definition\\)\\|if_statement\\|methods\\|properties\\|while_statement\\)\\'") parent 4)}>
    end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
    propertyCell = namedargs2cell(propArgs); %  <{Matched rule: ((parent-is "\\`function_definition\\'") parent matlab-ts-mode--set-function-indent-level-for-gp)}>
    bar(x, y, propertyCell{:}) %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
