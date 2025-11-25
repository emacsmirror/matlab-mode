% -*- matlab-ts -*- %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

myMatrix = [ %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
             1 2; %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
             3 4; %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
           ]; %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>

disp(myMatrix(1:  ... %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
              end)); %  <{Matched rule: ((parent-is "\\`range\\'") parent 0)}>

disp(myMatrix(1: (1 + ... %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
                  2))); %  <{Matched rule: ((parent-is "\\`binary_operator\\'") parent 0)}>

disp(myMatrix(1: [1 + ... %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
                  2])); %  <{Matched rule: ((parent-is "\\`binary_operator\\'") parent 0)}>
