% -*- matlab-ts -*- %  <{Matched rule: (maltab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

someStr = "[" + ... %  <{Matched rule: (maltab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
          "[[""A"", 1], [""B"", 2]]," + ... %  <{Matched rule: ((parent-is "\\`binary_operator\\'") parent 0)}>
          "[[""C"", 3]]," + ... %  <{Matched rule: ((parent-is "\\`binary_operator\\'") parent 0)}>
          "[[""D"", 4], [""E"", 5], [""F"", 6]]" + ... %  <{Matched rule: ((parent-is "\\`binary_operator\\'") parent 0)}>
          "]"; %  <{Matched rule: ((parent-is "\\`binary_operator\\'") parent 0)}>
