% -*- matlab-ts -*-

% t-utils-test-indent: no-line-by-line-indent - alignment of assignments not possible with line-by-line indent

% Calculate the Golden Ratio (phi)
               phi=  (     1+ sqrt(    5))/   2  ;
     disp(   [    'Golden Ratio (phi): ',        num2str( phi )  ]  );       % Display the value

   % Plot the exponential function with a Taylor Series Approximation
   x =  -2      :   0.1   :      2;
  y_exp=exp(    x  )  ;
      y_taylor = 1+x    +  x.^   2  /  2+x.^3     /6;    % First few terms

                       % Plot the approximation
   figure       ;% Create a new figure
   plot(    x,y_exp,           'b-','LineWidth',2);       % Plot the actual exponential
    hold on   ;% Keep the current plot
  plot(x, y_taylor,               ...
       'r--',...
'LineWidth', 1.5); 
title('Exponential Function & Taylor Approximation');
    xlabel(   'x');
ylabel('y');
   legend(   'exp(x)', 'Taylor Series'  );
  grid on  ;
 hold off  ;
