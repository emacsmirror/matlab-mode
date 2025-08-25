% -*- matlab-ts -*-
a = [1,2];


% Case1: (t-utils-xr (re-search-forward "'") (print (matlab-ts-mode--electric-pair-inhibit-predicate (char-before))))
a'

% Case2: (t-utils-xr (re-search-forward "'") (print (matlab-ts-mode--electric-pair-inhibit-predicate (char-before))))
b = "foo'bar"

% Case3: (t-utils-xr (re-search-forward "foo") (insert "'") (print (matlab-ts-mode--electric-pair-inhibit-predicate (char-before))) (delete-region (1- (point)) (point)))
s='foobar'

% start string
% Case4: (t-utils-xr (re-search-forward "s2") (insert " = '") (print (matlab-ts-mode--electric-pair-inhibit-predicate (char-before))) (delete-region (- (point) 4) (point)))
s2
