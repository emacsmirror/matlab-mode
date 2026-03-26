% -*- matlab-ts -*-

good1 = [1 [2, 3]
      4 5 6];

good2 = [1, 2, f(a,b), [1;];
      3 4 5 6.0e1];

str1 = ["foo,;" "bar[]"
        "ff;,", "goo..."];

function c=f(a,b)
c =a +b;
end
