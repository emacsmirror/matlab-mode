% -*- matlab-ts -*- %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

% t-utils-test-indent: no-line-by-line-indent - line-by-line typing results in error nodes %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

str1 = ["{"        ; %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
        "2"        ; %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
        'linethree'; %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
        '4'        ; %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
        'five'     ; %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
        '6.1'       %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
        "}"        ]; %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>


str2 = ["{"        , "foo"          ; %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
        "2"        , "b"            ; %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
        'linethree', "fooboar"      ; %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
        '4'        , "goo"          ; %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
        'five'     , "tooooo"       ; %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
        '6.1'      , "x"            ; %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
        "}"        , "zzzzzzzzzzzzz"]; %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
