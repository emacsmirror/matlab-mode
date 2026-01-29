% -*- matlab-ts -*-

% t-utils-test-indent: no-line-by-line-indent - line-by-line typing results in error nodes

function   [ pass , requirements] = electric_indent_strings (model )

s.foo = "foo";
s.(fieldName)  ;
    
requirements = {    message('a:b:c:foo').getString(  )};
            
b = find_system(model,'Name','Foo');

if strcmp(get_param(b,'BlockType'),'Abs')
    pass = 'yes';
else
    pass = 'no';
end

end
