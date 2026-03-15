% -*- matlab-ts -*- %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

% t-utils-test-indent: no-line-by-line-indent - we need all lines to align %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

cell1 = { %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
          [], [], [], []; % comment 1 %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
          [], [], [], []; % comment 2 %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
          0, 0, 0, 0;     % comment 3 %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
          0, 0, 0, 0;     % comment 4 %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
        }; %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>

cell2 = { % comment 1 %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
          [], [], [], []; % comment 2 %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
          [], [], [], []; % comment 3 %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
          0, 0, 0, 0;     % comment 4 %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
          0, 0, 0, 0;     % comment 5 %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
        }; %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>
