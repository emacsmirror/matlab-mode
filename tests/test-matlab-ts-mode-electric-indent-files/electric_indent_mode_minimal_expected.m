% -*- matlab-ts -*-

a = 1 + 2 * 3;

disp(a);

%-indent-mode=minimal

b = 1+2   * 3;

disp( b  );


%-indent-mode=full


c = 1 + 2 * 3;

disp(c);


% Following is misplaced (no prior minimal). This should casue an error.
%-indent-mode=full

d = 1 + 2 * 3;

disp(d);


%-indent-mode=minimal


% Following is misplaced (have prior minimal). This should casue an error.
%-indent-mode=minimal

e = 1+2   * 3;

disp( e  );
