% -*- matlab-ts -*- %  <{Matched rule: (maltab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

% t-utils-test-indent: no-line-by-line-indent - namespace typing can't be handled in context of ERROR %  <{Matched rule: (maltab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

function something = indent_fcn_cont %  <{Matched rule: (maltab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

    % Matched rule: ((parent-is "\\`arguments\\'") parent 0) %  <{Matched rule: ((parent-is "\\`function_definition\\'") parent matlab-ts-mode--set-function-indent-level-for-gp)}>
    something.foo1 = someFcn(1, ... %  <{Matched rule: ((parent-is "\\`function_definition\\'") parent matlab-ts-mode--set-function-indent-level-for-gp)}>
                             2); %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>

    something.foo3 = ... %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
        someFcn2(1, ... %  <{Matched rule: ((parent-is "\\`assignment\\'") parent 4)}>
                 2); %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>

    something.foo4 = ... %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
        someFcn2( ... %  <{Matched rule: ((parent-is "\\`assignment\\'") parent 4)}>
            1, ... %  <{Matched rule: (matlab-ts-mode--i-assign-cont-matcher matlab-ts-mode--i-assign-cont-anchor matlab-ts-mode--i-assign-cont-offset)}>
            2); %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>

    something.foo2 = someFcn(... %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
        1, ... %  <{Matched rule: (matlab-ts-mode--i-assign-cont-matcher matlab-ts-mode--i-assign-cont-anchor matlab-ts-mode--i-assign-cont-offset)}>
        2); %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>

    something.foo2 = 1 + someFcn(1, ... %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
                                 2); %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>

    something.foo2 = 1 + someFcn(... %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
                             1, ... %  <{Matched rule: ((node-is "\\`arguments\\'") parent 4)}>
                             2); %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>

    something.foo3 = namespace1.namespace2.widget.someFunction( ... %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
        other.thing); %  <{Matched rule: (matlab-ts-mode--i-assign-cont-matcher matlab-ts-mode--i-assign-cont-anchor matlab-ts-mode--i-assign-cont-offset)}>

    something.foo3 = 1 + namespace1.namespace2.widget.someFunction( ... %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
                             other.thing); %  <{Matched rule: ((n-p-gp "\\`arguments\\'" "\\`function_call\\'" "\\`field_expression\\'") grand-parent 4)}>

    something.foo4 = ... %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
        1 + ... %  <{Matched rule: ((parent-is "\\`assignment\\'") parent 4)}>
        namespace1.namespace2.widget.someFunction2( ... %  <{Matched rule: ((parent-is "\\`binary_operator\\'") parent 0)}>
            other.thing + ... %  <{Matched rule: ((n-p-gp "\\`arguments\\'" "\\`function_call\\'" "\\`field_expression\\'") grand-parent 4)}>
            bar2); %  <{Matched rule: ((parent-is "\\`binary_operator\\'") parent 0)}>

    something.foo4 =  ... %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
        1 + ... %  <{Matched rule: ((parent-is "\\`assignment\\'") parent 4)}>
        namespace1.namespace2.widget.someFunction2( ... %  <{Matched rule: ((parent-is "\\`binary_operator\\'") parent 0)}>
            other.thing + ... %  <{Matched rule: ((n-p-gp "\\`arguments\\'" "\\`function_call\\'" "\\`field_expression\\'") grand-parent 4)}>
            bar2); %  <{Matched rule: ((parent-is "\\`binary_operator\\'") parent 0)}>

    something.foo2 = someFcn(1) + ... %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
                     10 * (2 + ... %  <{Matched rule: ((parent-is "\\`binary_operator\\'") parent 0)}>
                           5); %  <{Matched rule: ((parent-is "\\`binary_operator\\'") parent 0)}>

    something.foo2 = someFcn(... %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
                         1, 2, ... %  <{Matched rule: ((node-is "\\`arguments\\'") parent 4)}>
                         3) + ... %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
                     10 * (2 + ... %  <{Matched rule: ((parent-is "\\`binary_operator\\'") parent 0)}>
                           5); %  <{Matched rule: ((parent-is "\\`binary_operator\\'") parent 0)}>

    something.foo2 = ... %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
        someFcn(... %  <{Matched rule: ((parent-is "\\`assignment\\'") parent 4)}>
            1) + ... %  <{Matched rule: ((node-is "\\`arguments\\'") parent 4)}>
        10 * (2 + ... %  <{Matched rule: ((parent-is "\\`binary_operator\\'") parent 0)}>
              5); %  <{Matched rule: ((parent-is "\\`binary_operator\\'") parent 0)}>

    something.foo4 = string(message( ... %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
                                "foo0123457890:foo0123457890:foo0123457890foo0123457890foo0123457890")); %  <{Matched rule: ((node-is "\\`arguments\\'") parent 4)}>
    something.foo4 = message( ... %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
        "foo0123457890:foo0123457890:foo0123457890foo0123457890foo0123457890"); %  <{Matched rule: (matlab-ts-mode--i-assign-cont-matcher matlab-ts-mode--i-assign-cont-anchor matlab-ts-mode--i-assign-cont-offset)}>

    something.foo4 = "foo" + string(message( ... %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
                                        "foo0123457890:foo0123457890:foo0123457890foo0123457890foo0123457890")); %  <{Matched rule: ((node-is "\\`arguments\\'") parent 4)}>

    something.foo4 = "foo" + ... %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
                     string(message( ... %  <{Matched rule: ((parent-is "\\`binary_operator\\'") parent 0)}>
                                "foo0123457890:foo0123457890:foo0123457890foo0123457890foo0123457890")); %  <{Matched rule: ((node-is "\\`arguments\\'") parent 4)}>

end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
