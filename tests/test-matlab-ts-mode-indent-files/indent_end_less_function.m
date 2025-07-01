% -*- matlab-ts -*-
function indent_end_less_function(a)
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
