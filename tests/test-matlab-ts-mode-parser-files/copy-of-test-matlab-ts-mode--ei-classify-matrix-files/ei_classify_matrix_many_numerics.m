% -*- matlab-ts -*-

M = [
              1 % Integer
            -42 % Negative integer
        3.14159 % Float (decimal)
       -2.71828 % Negative float
         1.23e5 % Exponential notation (123000)
       -4.56e-3 % Negative exponential (-0.00456)
         0b1010 % Binary (10 in decimal)
         0B0101 % Binary with uppercase B
           0xFF % Hexadecimal (255 in decimal)
         0X0005 % Hex with upper case X
            inf % Infinity
           -inf % Negative infinity
            nan % Not a Number
             pi % Built-in constant pi
        realmax % Largest positive float
        realmin % Smallest positive normalized float
            eps % Floating-point relative accuracy
          myFcn % Any valid matlab identifier should be treated as a numeric value
        5.0d-15; % Fortan style floating point numbers
        5.0D-15; % Fortan style floating point numbers


      % Signed/unsigned
      % ----
      % https://github.com/acristoffers/tree-sitter-matlab/issues/82 
      % https://www.mathworks.com/help/matlab/matlab_prog/specify-hexadecimal-and-binary-numbers.html
      % 
      % To specify unsigned 8-, 16-, 32-, and 64-bit integer types, use the suffixes u8, u16, u32,
      % and u64 (upper case U is also fine).

      % To specify signed 8-, 16-, 32-, and 64-bit integer types, use the suffixes s8, s16, s32, and
      % s64 (upper case S is also fine).

         0xFFs8
        0xFFs16
        0xFFs32
        0xFFs64

         0xFFu8
        0xFFu16
        0xFFu32
        0xFFu64

       0b0101s8
      0b0101s16
      0b0101s32
      0b0101s64

       0b0101u8
      0b0101u16
      0b0101u32
      0b0101u64

         0XFFS8
        0XFFS16
        0XFFS32
        0XFFS64

         0XFFU8
        0XFFU16
        0XFFU32
        0XFFU64

       0B0101S8
      0B0101S16
      0B0101S32
      0B0101S64

       0B0101U8
      0B0101U16
      0B0101U32
      0B0101U64

    ];
