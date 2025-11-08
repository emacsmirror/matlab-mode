% -*- matlab-ts -*-

%{
  Exercise inserting a blankline in an existing function call.

  Case1:
  (t-utils-xr
  
  (re-search-forward "in2 \\.\\.\\.")
  (re-search-backward " ")

  (insert ",")
  "C-e"

  "C-m"
  (insert "in3 ...")

  (re-search-backward "^fun")
  (t-utils-xr-print-code (point) (point-max))
  )

%}

function indent_xr_blank_line_in_fcn_call(in1, in2, in3)
    fprintf(1, '%s:%s:%s\n', ...
            in1, ...
            in2 ...
            );
end
