% -*- matlab-ts -*-

% foocmd0 could be a variable or a function
foocmd0

foocmd bar

% Following is an error, though matlab tree-sitter parses it as if MATLAB command-dual allowed
% multiple arguments.
foocmd2 bar goo

% system "!" command supports multiple arguments
! ls *.m *.txt
