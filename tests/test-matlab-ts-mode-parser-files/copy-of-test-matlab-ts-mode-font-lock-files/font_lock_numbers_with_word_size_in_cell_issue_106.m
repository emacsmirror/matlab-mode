% -*- matlab-ts -*-

% See: https://github.com/acristoffers/tree-sitter-matlab/issues/106

% Doc https://www.mathworks.com/help/matlab/matlab_prog/specify-hexadecimal-and-binary-numbers.html
% 
% To specify unsigned 8-, 16-, 32-, and 64-bit integer types, use the suffixes u8, u16, u32, and u64.
% To specify signed 8-, 16-, 32-, and 64-bit integer types, use the suffixes s8, s16, s32, and s64.

a = {0xFFs8, 1}
b = {0xFFs16, 1}
c = {0xFFs32, 1}
d = {0xFFs64, 1}

e = {0xFFu8, 1}
f = {0xFFu16, 1}
g = {0xFFu32, 1}
h = {0xFFu64, 1}

i = {0b0101s8, 1}
j = {0b0101s16, 1}
k = {0b0101s32, 1}
l = {0b0101s64, 1}

m = {0b0101u8, 1}
n = {0b0101u16, 1}
o = {0b0101u32, 1}
p = {0b0101u64, 1}

