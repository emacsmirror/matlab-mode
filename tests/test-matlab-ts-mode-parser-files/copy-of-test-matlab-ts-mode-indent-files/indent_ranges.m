% -*- matlab-ts -*-

myMatrix = [ 
             1 2;
             3 4;
           ];

disp(myMatrix(1:  ...
              end)); 

disp(myMatrix(1: (1 + ...
                  2)));

disp(myMatrix(1: [1 + ...
                  2]));
