% -*- matlab-ts -*-

% t-utils-test-indent: no-line-by-line-indent - can't indent typing line-by-line
% because on start, we assume an empty file has function indent level of 4

function indent_endless_function(a)
% function without a terminating end

x = {'a', 'c()' 'b'};
if a
    disp('foo');
else
    disp('bar');
end

indent_end_less_function2

function indent_end_less_function2
disp('in indent_end_less_function2')
