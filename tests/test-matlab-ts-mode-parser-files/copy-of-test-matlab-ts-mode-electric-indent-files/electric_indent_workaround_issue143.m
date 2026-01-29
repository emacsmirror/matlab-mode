% -*- matlab-ts -*-

% t-utils-test-indent: no-line-by-line-indent - line-by-line typing results in error nodes

% workaround https://github.com/acristoffers/tree-sitter-matlab/issues/143

if 1
               a=1234./2+500./4+600.'+700.\1+800.*2+sum([444.,555.]);   
end

x = 123.* 4 + 567

% x = 123.*4 + 567

y = 123.*4 + 567
