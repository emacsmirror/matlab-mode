% -*- mode: matlab-ts -*-

% Single line comment

%{
   multiline
   comment
%}

% Note, "%{" followed by text is a single line comment, e.g. in following,
% myVar has value 1 and the statement is the same as: myVar = 1

myVar = 1 %{single line comment %} + 1;

% This is also a single line comment:

%{ single line comment

% https://github.com/acristoffers/tree-sitter-matlab/issues/37
%{
%}
