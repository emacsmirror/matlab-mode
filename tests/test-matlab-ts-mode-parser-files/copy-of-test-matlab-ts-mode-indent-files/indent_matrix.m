% -*- matlab-ts -*-
function indent_matrix
    a = [ ...
          1 ...
          + ...
          2
        ];
    disp(a);

    a = [    2 ...
             1 ...
        ];
    disp(a);

    a = [1, 2;
         3, 4];
    disp(a);

    a = [ ...
          2 + [ 3
                4,
                5 + [ ...
                      2
                    ]
              ]
        ];
    disp(a);

    a = [ ...
          1; ...
          2 ...
        ];
    disp(a);

    long_variable_a = ...
        [
          2, 123, 456
          3,   2    7
        ];
    disp(long_variable_a);

end
