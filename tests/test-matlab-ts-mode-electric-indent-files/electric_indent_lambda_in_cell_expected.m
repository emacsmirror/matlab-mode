% -*- matlab-ts -*-

% From https://www.mathworks.com/help/matlab/matlab_prog/anonymous-functions.html, we have:
%    Arrays of Anonymous Functions
%
%    Although most MATLAB fundamental data types support multidimensional arrays, function handles must
%    be scalars (single elements). However, you can store multiple function handles using a cell array
%    or structure array. The most common approach is to use a cell array, such as
%
%    f1 = {@(x)x.^2;
%         @(y)y+10;
%         @(x,y)x.^2+y+10};
%
%    When you create the cell array, keep in mind that MATLAB interprets spaces as column
%    separators. Either omit spaces from expressions, as shown in the previous code, or enclose
%    expressions in parentheses, such as
%
%    f2 = {@(x) (x.^2);
%          @(y) (y + 10);
%          @(x,y) (x.^2 + y + 10)};
%
% However, this does not appear to be true. Using
%
%    f3 = {@(x) x.^2;
%          @(y) y + 10;
%          @(x, y) x.^2 + y + 10};
%
% works same as f1.

% t-utils-test-indent: no-line-by-line-indent - line-by-line typing results in error nodes

f = {@(x) x.^2;
     @(y) y + 10;
     @(x, y) x.^2 + y + 10};
