% -*- matlab-ts -*-

function electric_ends_if_continued_with_end(a)

    %{
      Case1:
      (t-utils-xr
      (re-search-forward "%}") "C-n"

      "C-i"
      (insert "if a > 1")                     "C-m"

      "C-p" "C-e"

      (insert " && ...")                      "C-m"
      "C-k"
      (insert "a < 10")                       "C-m"
      (insert     "disp('a > 1 && a < 10')")

      (re-search-backward "^    if")
      (t-utils-xr-print-code (point) (re-search-forward "^end\n"))
      )
    %}

end
