% -*- matlab-ts -*- %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

function y = < % function_name %>( x ) %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

               < %- castExpression = '';  %could be uint32 for e.g. %> %  <{Matched rule: (matlab-ts-mode--i-next-line-matcher matlab-ts-mode--i-next-line-anchor matlab-ts-mode--i-next-line-offset)}>
                 x_idx = x*<% coder.internal.tools.TML.tostr(deltaXInv) %>; %  <{Matched rule: (matlab-ts-mode--i-next-line-matcher matlab-ts-mode--i-next-line-anchor matlab-ts-mode--i-next-line-offset)}>

                             case 0 %> %  <{Matched rule: (matlab-ts-mode--i-next-line-matcher matlab-ts-mode--i-next-line-anchor matlab-ts-mode--i-next-line-offset)}>
                               idx_bot = <% castExpression %>(<% [function_name,'_round']%>(x_idx)) + 1; %  <{Matched rule: (matlab-ts-mode--i-next-line-matcher matlab-ts-mode--i-next-line-anchor matlab-ts-mode--i-next-line-offset)}>

end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>

