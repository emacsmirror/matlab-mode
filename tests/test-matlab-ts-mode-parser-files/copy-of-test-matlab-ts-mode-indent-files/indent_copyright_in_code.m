% -*- matlab-ts -*-

% t-utils-test-indent: no-line-by-line-indent - multiline comments cannot be indented correctly
% when typing line-by-line because we don't know where in a multiline comment until we see the end.

   function b = indent_copyright_in_code
   %{
     sadf
   
         asdfasd     
     %}
       % foo
    % foo
   
       
       
    % copyright blah
   
     % foo
          b = 1;
 end
