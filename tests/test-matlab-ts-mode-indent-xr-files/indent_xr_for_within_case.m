% -*- mode: matlab-ts; matlab-ts-mode-electric-ends: nil -*-


%{

  (t-utils-xr

  (re-search-forward ":10")                  "C-m"
  (insert "disp('i:');")                     "C-m"
  (insert "disp(i);")                        "C-m"
  (insert "end")
  "C-i"

  (re-search-backward "^fun")
  (t-utils-xr-print-code (point) (point-max))

  )
%}

function indent_xr_for_within_case(completions)
    for cIdx = 1 : length(completions)
        switch completions{cIdx}
          case {'foo'}
            for i=1:10
          otherwise
            error('assert - unhandled entryType');
        end
    end
end

