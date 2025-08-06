% -*- matlab-ts -*-

%{
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

  (re-search-backward "^fun")
  (t-utils-xr-print-code (point) (point-max))
  )
%}
