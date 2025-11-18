% -*- matlab-ts -*- %  <{Matched rule: (maltab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

spmd %  <{Matched rule: (maltab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
    % foo %  <{Matched rule: ((parent-is "\\`spmd") parent 4)}>
    q = magic(spmdIndex + 2); %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>

myClusterPool = parpool("myMJSCluster",15); %  <{Matched rule: (maltab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

spmdWorkers = myClusterPool.Workers(1:6); %  <{Matched rule: (maltab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
[spmdPool,otherPool] = partition(myClusterPool,"Workers",spmdWorkers); %  <{Matched rule: (maltab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

fun = @(x) 4./(1 + x.^2); %  <{Matched rule: (maltab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
spmd(spmdPool) %  <{Matched rule: (maltab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
    a = (spmdIndex - 1)/spmdSize; %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
    b = spmdIndex/spmdSize; %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
    myIntegral = integral(fun,a,b); %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
    piApprox = spmdPlus(myIntegral); %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
