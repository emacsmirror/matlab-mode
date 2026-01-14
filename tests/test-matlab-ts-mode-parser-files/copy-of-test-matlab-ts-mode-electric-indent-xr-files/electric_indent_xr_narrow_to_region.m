% -*- matlab-ts -*-

%{
  Case1:
  (t-utils-xr

  (re-search-forward "a{") "C-a"
  "C-a"
  "C-@"
  "C-n"
  "C-n"
  (narrow-to-region (mark) (point))
  "C-@" ;; unset mark
  "C-x h"  ;; select all
  (indent-region (point-min) (point-max))
  (widen)

  (re-search-backward "^fun")
  (t-utils-xr-print-code (point) (point-max))
      
  )
%}

function electric_indent_xr_narrow_to_region(a, longInputVar, c)
arguments
    a{                 mustBeNumeric, mustBeReal   }
        longInputVar { mustBeNumeric, mustBeReal, mustBeFinite}
                  c{mustBeNumeric}
    end
end
