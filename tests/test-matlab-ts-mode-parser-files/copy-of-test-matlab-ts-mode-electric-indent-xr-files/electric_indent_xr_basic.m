% -*- matlab-ts -*-

if 1
    % Case1: (t-utils-xr (re-search-forward "2  ") "C-i")
    v1 = 1     ./ [2     42222];

    % Case2: (t-utils-xr (re-search-forward "42") (backward-char 2) "C-i")
    v2 = 1     ./ [2     42222];

    % Case3: (t-utils-xr (re-search-forward "v3") (backward-char 2) "C-i")
    v3 = 1     ./ [2     42222];

    % Case4: (t-utils-xr (re-search-forward "22    ") (backward-char 2) "C-i")
    v4 = 2 *   [11 22    
           33  44];

    % Case5: (t-utils-xr (re-search-forward "33 ") (backward-char 2) "C-i")
    v5 = 2 *   [11 22    
           33  44];
end
