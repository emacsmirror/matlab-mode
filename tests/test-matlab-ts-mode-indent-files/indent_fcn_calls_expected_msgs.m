% -*- matlab-ts -*- %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
var_a = my_function(1, 2, 3); %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

var_b = my_function(1, ... %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
                    2, ... %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
                    3); %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>

my_struct.var_c = my_function( ... %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
    1, ... %  <{Matched rule: (matlab-ts-mode--i-assign-cont-matcher matlab-ts-mode--i-assign-cont-anchor matlab-ts-mode--i-assign-cont-offset)}>
    2, ... %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
    3); %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>

my_other_function(1, ... %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
                  2, ... %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
                  3); %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>

% some extra spaces after a function call %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
my_other_function  ( ... %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
    1, ... %  <{Matched rule: ((node-is "\\`arguments\\'") parent 4)}>
    2, ... %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
    3); %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
