% -*- matlab-ts -*-

function y = < % function_name %>( x )

               < %- castExpression = '';  %could be uint32 for e.g. %>
                 x_idx = x*<% coder.internal.tools.TML.tostr(deltaXInv) %>;

                             case 0 %>
                               idx_bot = <% castExpression %>(<% [function_name,'_round']%>(x_idx)) + 1;

end

