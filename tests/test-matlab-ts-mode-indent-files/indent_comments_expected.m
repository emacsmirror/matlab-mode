% -*- matlab-ts -*-

% t-utils-test-indent: no-line-by-line-indent - not possible to indent line-by-line
% because of the multi-line comments.

function b = indent_comments(a)
% this the doc help
% comment

    % comment about fcn1
    [c, d] = fcn1(a, a);

    b = c + d;

    % comment about fcn2
    fcn2;
end


function [c, d] = fcn1(a, b)
%{
  help comment

  with blank lines

%}
    c = a;
    d = b;
end


function fcn2
%{
  help comment

  with blank lines


%}

    %{

      block
      comment
      for following

      line
    %}

    disp('2');
end
