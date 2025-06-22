function syntax_table_test1
% help comment

    % code comment
    x = "double quote string";
    y = 'double quote string';
    x = [
          1 2; ...
          3 4; ... comment
        ];
    x = x';
    x = x'';
    y = x(2, 2);
end
