% -*- matlab-ts -*- %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

% t-utils-test-indent: no-line-by-line-indent - when typing matrix line-by-line, there are %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
% error nodes and thus the matrix alignment doesn't occur %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher matlab-ts-mode--i-block-comment-end-anchor 0)}>

mNonNumeric1 = [ %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
                 1, a + b %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
                 3, a - b %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
               ]; %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>

mNonNumeric2 = [ %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
                 1, a + b %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
                 3, a - b %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
               ]; %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>

mNonNumeric3 = [ %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
                 1, a + b %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
                 3, a - b %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
               ]; %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>
