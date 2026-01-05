% -*- matlab-ts -*- %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

% Exercise matlab-ts-mode--ei-align-line-in-m-matrix %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

% t-utils-test-indent: no-line-by-line-indent - when typing matrix line-by-line, there are %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
% error nodes and thus the matrix alignment doesn't occur %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher parent 0)}>

m = [1111,     2, 223 %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
      ... %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
        4, 53333,   6]; %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>


m1 = [ ... %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
         1 2222; ... %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
       ... %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
       333    4; ... %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
     ]; %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>

m1b = [ %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
          1 2222 % comment 1 %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
        333    4 % comment 2 %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
      ]; %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>

m2 = [ ... %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
         1 2222 %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
       333    4 %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
     ]; %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>

m3a = [  21,     2, 223; %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
          4, 53333,   6; ... %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
       1222,     4,   5]; %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>

m3b = [1111,     2, 223; %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
          4, 53333,   6; ... %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
       1222,     4,   5;]; %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>

m3c = [1111,   2, 223; %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
          4, 533,   6; ... %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
       1222,   4,   5;]; %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>

a = 1; %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
b = 2; %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
c = 3; %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
d = 4; %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

m4a = [a * b + c, d + a; %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
               b,     c]; %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>

m4b = [a * b + c, d + a; %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
               b,     c]; %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>

m5a = [a * b + c d + a; ... %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
        ... %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
        ... %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
               b     c]; %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>

m6 = [ ... %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
       ... %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
         1 2222 %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
       ... %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
       ... comment %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
       333    4 %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
     ]; %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>

m7 = [   1111; ... %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
           22; ... %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
          333; ... %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
      1111111]; %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>



not1 = [1111, ... %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
        22; ... %  <{Matched rule: ((parent-is "\\`\\(?:function_output\\|row\\)\\'") parent 0)}>
        333, ... %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
        1111111]; %  <{Matched rule: ((parent-is "\\`\\(?:function_output\\|row\\)\\'") parent 0)}>

not2 = [1 2 223; %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
        [4 53333 6]; ... %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
        1222 4 5; ... %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
       ]; %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>

not3 = [ ... %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
         1 2 3; 4 5 6; ... %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
         1 4 5; ... %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
       ]; %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>

not4 = [a * b + c, [d + a]; %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
        b, c]; %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>

not5 = [ ... %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
         1 2222; ... %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
         333 4; ... %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
       ]; if 1, disp('here') %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>
          end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>

not6 = [1 2; %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
        3 4 5]; %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
