% -*- matlab-ts -*- %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

mat = [ %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
        a.b.c() %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
        {'logical'} %  <{Matched rule: ((n-p-gp "\\`[({]\\'" nil "\\`field_expression\\'") grand-parent 0)}>
        a.b.c.(d) %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
        {'other'} %  <{Matched rule: ((n-p-gp "\\`[({]\\'" nil "\\`field_expression\\'") grand-parent 0)}>
        a.b.c{d} %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
        {'other'} %  <{Matched rule: ((n-p-gp "\\`[({]\\'" nil "\\`field_expression\\'") grand-parent 0)}>
      ]; %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>
