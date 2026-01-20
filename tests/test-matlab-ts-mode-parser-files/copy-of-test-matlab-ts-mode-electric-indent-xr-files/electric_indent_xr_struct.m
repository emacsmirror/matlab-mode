% -*- matlab-ts -*-

%{
  case1-point-before-comma:
  (t-utils-xr

  (re-search-forward "fiel")  ;; point left on the "d"
  "C-i"
  "C-n" "C-i"
  "C-n" "C-i"
  )

%}

if 1
       s1 = struct(     'field1', value1 + 10, ...
                ...
  'otherfield2', value2);
end

%{
  case2-point-after-comma:
  (t-utils-xr

  (re-search-forward "value")  ;; point left on the 9
  "C-i"
  "C-n" "C-i"
  "C-n" "C-i"
  )

%}

if 1
       s2 = struct(     'field1', value9 + 10, ...
                ...
  'otherfield2', value2);
end

% case3: (t-utils-xr (t-utils-xr-print-code (point-min) (point-max)))
