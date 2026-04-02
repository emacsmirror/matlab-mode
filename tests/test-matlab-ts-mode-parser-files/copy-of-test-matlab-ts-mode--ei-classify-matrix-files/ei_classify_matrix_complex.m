% -*- matlab-ts -*-

good1 = [1 [2, 3]
      4 5 6];

good2 = [1, 2, f(a, b), 2 * sum([1, 5, 7, 11 : 100]);
         3, 4, 5, 6.0e1];

str1 = ["foo,;" "bar[]"
        "ff;,", "goo..."];

function c=f(a,b)
c =a +b;
end
