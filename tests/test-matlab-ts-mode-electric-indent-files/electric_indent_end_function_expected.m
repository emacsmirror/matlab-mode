% -*- matlab-ts -*-

% t-utils-test-indent: no-line-by-line-indent - when typing line-by-line we can't align

function out = electric_indent_end_function(in)
    b   = in.begin();
    e   = in.end();
    out = e - b;
end
