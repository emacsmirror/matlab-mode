% -*- matlab-ts -*-

% see https://github.com/acristoffers/tree-sitter-matlab/issues/81

Name = ["foo";"bar"];
Age = [38;43];
tbl = table(Name, Age);
x=tbl.(1)

