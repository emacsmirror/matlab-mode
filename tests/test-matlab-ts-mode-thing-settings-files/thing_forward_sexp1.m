% -*- matlab-ts -*-
function mat = thing_forward_sexp1
    % Case1: (t-utils-xr "C-n" "C-n" "C-e" "C-b" (forward-sexp -1) (forward-sexp) "C-b" (forward-sexp -1) (forward-sexp))
    m = [[1, 2];
           [3, 4]];

    % Case2: (t-utils-xr "C-n" "C-n" "C-e" "C-b" (forward-sexp -1) (forward-sexp) "C-b" (forward-sexp -1) (forward-sexp))
    c = {{1, 2};
         {3, 4}};
    
    % Case3: (t-utils-xr "C-n" "C-n" "C-e" "C-b" (forward-sexp -1) (forward-sexp) "C-b" (forward-sexp -1) (forward-sexp))
    v = ((1+2) * ...
         (3+4));

    % Case4: (t-utils-xr "C-n" "C-e" (forward-sexp -1) (forward-sexp) "C-b" (forward-sexp -1) (forward-sexp))
    %  Comment with paren's ((1+2)*(3+4))
end
