% -*- mode: matlab-ts; matlab-ts-mode-electric-ends: nil -*-

%{

  Note: below we don't indent the "b ... comment for b" line correctly because the parse tree
  doesn't support this. See
  tests/test-matlab-ts-mode-indent-files/indent_fcn_ellipsis.skip.typing.txt

  Case1:
  (t-utils-xr

  (re-search-forward "%}") "C-n"

  (insert "\n") "C-b"  ;; ensure we have a newline

  (insert "function ...")                  "C-m"
  (insert     "[ ...")                     "C-m"
  (insert      "b ... comment for b")      "C-m"
  (insert     "] = ...")                   "C-m"
  (insert     "indent_xr_fun5 ...")        "C-m"
  (insert     "( ...")                     "C-m"
  (insert      "a ... comment for a")      "C-m"
  (insert     ")")                         "C-m"
  "C-m"
  (insert     "b=2*a;")                    "C-m"
  (insert "end")                           "C-m"

  (indent-region (point-min) (point-max))
  (re-search-backward "^fun")
  (t-utils-xr-print-code (point) (point-max))
  )
%}
