% -*- matlab-ts -*- %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

% t-utils-test-indent: no-line-by-line-indent - line-by-line typing results in error nodes %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

if 1 %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
    s1 = struct('field1',      value1 + 10, ... %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
                ... %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
                'otherfield2', value2); %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>

s2 = struct('field1',      value1, ... %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
            'otherfield2', value2) %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>

a(1, 1) = struct('field1',      value1, ... %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
                 'otherfield2', value2) %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>

a(1, 2) = struct( ... %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
    'field1',      value1, ... %  <{Matched rule: (matlab-ts-mode--i-assign-cont-matcher matlab-ts-mode--i-assign-cont-anchor matlab-ts-mode--i-assign-cont-offset)}>
    'otherfield2', value2, ... %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
    'foo',         1 + 2 * 3) %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>

s3 = struct( ... %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
    'field1',      value1, ... %  <{Matched rule: (matlab-ts-mode--i-assign-cont-matcher matlab-ts-mode--i-assign-cont-anchor matlab-ts-mode--i-assign-cont-offset)}>
    'otherfield2', value2); %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>

s4 = struct( ... %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
    'longField1', value1, ... %  <{Matched rule: (matlab-ts-mode--i-assign-cont-matcher matlab-ts-mode--i-assign-cont-anchor matlab-ts-mode--i-assign-cont-offset)}>
    'field2',     value2) %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>

s5 = struct("a",      1, ... %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
            ... %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
            "foobar", (2 + 3) * 4 + 5, ... %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
            "g",      3); %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>

not1 = struct('one', value1, 'two', value2, ... %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
              'three', value3); %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>

not2 = struct(['field1', 'foo'], value1 + 10, ... %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
              ... %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
              'otherfield2', value2); %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>

% Comma in field name not allowed because that isn't supported (yet) by matlab-ts-mode--ei.el %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
% In matlab-ts-mode--ei.el, we assume (string-match-p "," ei-line open-paren-offset) goes %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher matlab-ts-mode--i-block-comment-end-anchor 0)}>
% to the comma after the first field. To support fields with commas in the name, we'll need %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher matlab-ts-mode--i-block-comment-end-anchor 0)}>
% to do more work and commas are a real edge case. %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher matlab-ts-mode--i-block-comment-end-anchor 0)}>
not3 = struct('field1,foo', value1 + 10, ... %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
              ... %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
              'otherLongField2', value2); %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>

not5 = struct ... %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
    ('field1', 1, ... %  <{Matched rule: (matlab-ts-mode--i-assign-cont-matcher matlab-ts-mode--i-assign-cont-anchor matlab-ts-mode--i-assign-cont-offset)}>
     'longField2', 2); %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>

not6 = ... %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
    struct ... %  <{Matched rule: ((parent-is "\\`assignment\\'") parent 4)}>
        ('field1', 1, ... %  <{Matched rule: (matlab-ts-mode--i-assign-cont-matcher matlab-ts-mode--i-assign-cont-anchor matlab-ts-mode--i-assign-cont-offset)}>
         'lognField2', 2); %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>

not7 ... %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
    = ... %  <{Matched rule: ((parent-is "\\`assignment\\'") parent 4)}>
    struct ... %  <{Matched rule: ((parent-is "\\`assignment\\'") parent 4)}>
        ('field1', 1, ... %  <{Matched rule: (matlab-ts-mode--i-assign-cont-matcher matlab-ts-mode--i-assign-cont-anchor matlab-ts-mode--i-assign-cont-offset)}>
         'longField2', 2); %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>

not8 = struct; % (this shouldn't be aligned! %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
