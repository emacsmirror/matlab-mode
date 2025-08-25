% -*- matlab-ts -*-

spmd
    % foo
    q = magic(spmdIndex + 2);
end

myClusterPool = parpool("myMJSCluster",15);

spmdWorkers = myClusterPool.Workers(1:6);
[spmdPool,otherPool] = partition(myClusterPool,"Workers",spmdWorkers);

fun = @(x) 4./(1 + x.^2);
spmd(spmdPool)
    a = (spmdIndex - 1)/spmdSize;
    b = spmdIndex/spmdSize;
    myIntegral = integral(fun,a,b);
    piApprox = spmdPlus(myIntegral);
end
