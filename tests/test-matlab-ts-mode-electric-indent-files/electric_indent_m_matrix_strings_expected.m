% -*- matlab-ts -*-

% t-utils-test-indent: no-line-by-line-indent - line-by-line typing results in error nodes

str1 = ["{"        ;
        "2"        ;
        'linethree';
        '4'        ;
        'five'     ;
        '6.1'
        "}"        ];


str2 = ["{"        , "foo"          ;
        "2"        , "b"            ;
        'linethree', "fooboar"      ;
        '4'        , "goo"          ;
        'five'     , "tooooo"       ;
        '6.1'      , "x"            ;
        "}"        , "zzzzzzzzzzzzz"];
