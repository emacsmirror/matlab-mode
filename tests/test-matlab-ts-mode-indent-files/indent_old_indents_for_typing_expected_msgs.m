% -*- matlab-ts -*- %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

%  To be used for "typing" line-by-line. Same as ./indent_old_indents.m, but no %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
%  %{ block comments %} and no nested functions. These do not support indenting line-by-line. %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher matlab-ts-mode--i-block-comment-end-anchor 0)}>

function indent_old_indents_for_typing(a,b,stuff,cmddual1fake,cmddual2fake) %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
% Help text %  <{Matched rule: (matlab-ts-mode--i-doc-comment-matcher matlab-ts-mode--i-doc-comment-anchor matlab-ts-mode--i-doc-comment-offset)}>
% !!0 %  <{Matched rule: (matlab-ts-mode--i-doc-comment-matcher matlab-ts-mode--i-doc-comment-anchor matlab-ts-mode--i-doc-comment-offset)}>
% of many lines %  <{Matched rule: (matlab-ts-mode--i-doc-comment-matcher matlab-ts-mode--i-doc-comment-anchor matlab-ts-mode--i-doc-comment-offset)}>
% !!0 %  <{Matched rule: (matlab-ts-mode--i-doc-comment-matcher matlab-ts-mode--i-doc-comment-anchor matlab-ts-mode--i-doc-comment-offset)}>

    % including a gap - comment for following code %  <{Matched rule: ((parent-is "\\`function_definition\\'") parent matlab-ts-mode--set-function-indent-level-for-gp)}>
    % !!4 %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher matlab-ts-mode--i-block-comment-end-anchor 0)}>

    arguments (Repeating) % !!4 %  <{Matched rule: ((parent-is "\\`function_definition\\'") parent matlab-ts-mode--set-function-indent-level-for-gp)}>
        a (1,1) {mustBeNumeric}                                 % !!8 %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
        b (:,:) double                                          % !!8 %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
        stuff {mustBeMember(stuff, { 'this' 'that' 'other' })}  % !!8 %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
        cmddual1fake double  % !!8 %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
        cmddual2fake int     % !!8 %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
    end % !!4 %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>

    persistent var1 % !!4 %  <{Matched rule: ((parent-is "\\`function_definition\\'") parent matlab-ts-mode--set-function-indent-level-for-gp)}>
    global     var2 % !!4 %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
    persistent var3 % !!4 %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>


    locala = a; %#ok %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
    localb = b; %#ok %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
    localstuff = stuff; %#ok %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>

    if isempty(var1) var1=1; end %#ok !!4 %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
    if isempty(var3) var3=2; end %#ok !!4 %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>

    ends_in_comments_and_strings(var1, var2, var3); % !!4 has end in name %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>

    % !!4 %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>

    block_starts_in_comments_and_strings(cmddual1fake,cmddual2fake); %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
    array_constant_decls(); %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>

    % !!4 %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>

    continuations_and_block_comments(); %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>

    % $$$ !!0 %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
    % $$$ special ignore comments %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher matlab-ts-mode--i-block-comment-end-anchor 0)}>

    has_nested_fcn(); % !!4 %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>

    % !!4  - after ignore comments %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>

end % Comment with end in it %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>

%!!0 %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

function B = ends_in_comments_and_strings() %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
% !!0 %  <{Matched rule: (matlab-ts-mode--i-doc-comment-matcher matlab-ts-mode--i-doc-comment-anchor matlab-ts-mode--i-doc-comment-offset)}>

    % >>6 %  <{Matched rule: ((parent-is "\\`function_definition\\'") parent matlab-ts-mode--set-function-indent-level-for-gp)}>
    if foo %  <{Matched rule: ((parent-is "\\`function_definition\\'") parent matlab-ts-mode--set-function-indent-level-for-gp)}>
        A = 1; %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
    end % <<6 end in comment after end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>

    % !!4 %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>

    symbol_with_end_in_it; %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>

    B = A(1:end); %#ok %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>

    %% cell start comment !!4 %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
    if foo %!!4 %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
        C = "this is the end of the line"; %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
        % !!8 %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
    else %!!4 %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>

        % !!8 %  <{Matched rule: ((parent-is "\\`\\(?:else\\(?:\\(?:if\\)?_clause\\)\\)\\'") parent 4)}>
    end;  D = "string end string"; %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
    % !!4 %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>

    E = [ D C]; %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>

    if bar %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>

        A = E; %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>

    end; B = A(1:end); %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
    % !!4 %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>

    E = B; %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>

    if baz %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>

        A = C; %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>

    end; B = [ 1 2 ...  % is this the end? %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
               3 4 ];   % !!15 %  <{Matched rule: ((parent-is "\\`\\(?:function_output\\|row\\)\\'") parent 0)}>

    % !!4 %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>

    if foo %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>

        A = E; %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>

    end % the other end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
    % !! 4 %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher matlab-ts-mode--i-block-comment-end-anchor 0)}>

    B = [ B A ]; % !!4 %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>

    str = 'This is a char array with ... in it'; %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
    foo(str); % !!4 %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>

    fcncall(arg1, '...', arg3); % !!4 %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
    1; % !!4 %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>

    % Multi- end s %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
    % >>8 %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher matlab-ts-mode--i-block-comment-end-anchor 0)}>
    if foo %#ok %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
        if bar %#ok %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
            if baz %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>

                A = B; %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>

            else %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>

            end; end; end % <<8 comment end thing %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>

    % !!4 %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
    B = A; %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>

end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>

function out = array_constant_decls() %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

    A = [ 1 2 3 ]; %!!4 %  <{Matched rule: ((parent-is "\\`function_definition\\'") parent matlab-ts-mode--set-function-indent-level-for-gp)}>

    Blong = [ 1 2; %!!4 %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
              3 4; %!!14 %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
            ]; %!!12 %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>

    Csep = [ %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
             1 2; %!!8 %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
             3 4; %!!8 %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
           ]; %!!11 %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>

    multinest = { [ 1 2               %!!4 %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
                    3 4 ];            %!!20 %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
                  { 5 6 7 ...         %!!18 %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
                    8 9 10 ...        %!!20 %  <{Matched rule: ((parent-is "\\`\\(?:function_output\\|row\\)\\'") parent 0)}>
                  };                  %!!18 %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>
                  fcncall(10, ...     %!!18 %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
                          12, ...     %!!26 %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
                          [ 13 14;    %!!26 %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
                            15 16 ])  %!!28 %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
                } ;  %!!16 %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>

    nest = { ... %!!4 %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
             1        %!!8 %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
             [ ...    %!!8 %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
               2 3    %!!10 %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
             ] ...    %!!8 %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>
             3        %!!8 %  <{Matched rule: ((parent-is "\\`\\(?:function_output\\|row\\)\\'") parent 0)}>
           };    %!!11 %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>

    cascade_long_name = ... %!!4 %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
        { ...               %!!8 %  <{Matched rule: ((parent-is "\\`assignment\\'") parent 4)}>
          1                 %!!10 %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
          2                 %!!10 %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
        };                  %!!8 %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>

    % TODO %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
    % I don't know why the below indents this way. %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher matlab-ts-mode--i-block-comment-end-anchor 0)}>
    % It should either do all max indent, or all lined up with parens. %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher matlab-ts-mode--i-block-comment-end-anchor 0)}>
    thing.thing.long.long.longname({ 'str' %!!4 %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
                                     'str' %!!37 %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
                                     'str' %!!37 %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
                                     'str' %!!37 %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
                                   });   %!!35 %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>

    thing.thing.long.long.longname('str', ... %!!4 %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
                                   'str', ... %!!35 %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
                                   'str', ... %!!35 %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
                                   'str' ...  %!!35 %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
                          );   %!!34 %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>

    % Line starting with end inside parens %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
    disp(Csep(1:  ...  %!!4 %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
              end));   %!!14 %  <{Matched rule: ((parent-is "\\`range\\'") parent 0)}>

    % This array has bad syntactic expression parsing due to the %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
    % apostrophy %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher matlab-ts-mode--i-block-comment-end-anchor 0)}>
    Closures = [ %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
                 755009 ; ... % 21-Feb-2067 Washington's Birthday (Mon) %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
                 755010 ;     % !!8 %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
               ]; %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>

    dep = [ %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
            root(info.function, factory, workspace, []), ...    % likewise this isn't a keyword %  <{Matched rule: ((parent-is "\\`\\(?:cell\\|matrix\\)\\'") parent 2)}>
            fcn3.finalize                                       % the single quote used to break [] scanning %  <{Matched rule: ((parent-is "\\`\\(?:function_output\\|row\\)\\'") parent 0)}>
          ]; %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>

    % This long fcn name last symbol starts with 'get' which %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
    % used to confuse and move to indent past 1st arg. %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher matlab-ts-mode--i-block-comment-end-anchor 0)}>
    if qesimcheck.utils.GetYesNoAnswer('Do ',... !!4 %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
                                       'n',...  !!39 %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
                                       'once') %!!39 %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
        code();  %!!8 %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
    end  %!!4 %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>



    % !!4 %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
    out = { A     %!!4 %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
            Blong %!!12 %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
            Csep  %!!12 %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
            nest  %!!12 %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
            multinest%!!12 %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
            cascade_long_name%!!12 %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
            Closures%!!12 %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
            dep %!!12 %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>
          };      %!!10 %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>

end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>

function C = block_starts_in_comments_and_strings(varargin) %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
% !!0 %  <{Matched rule: (matlab-ts-mode--i-doc-comment-matcher matlab-ts-mode--i-doc-comment-anchor matlab-ts-mode--i-doc-comment-offset)}>

    C = 0; %  <{Matched rule: ((parent-is "\\`function_definition\\'") parent matlab-ts-mode--set-function-indent-level-for-gp)}>

    if varargin{1} % if true %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>

        % !!8 %  <{Matched rule: ((parent-is "\\`\\(?:arguments_statement\\|e\\(?:numeration\\|vents\\)\\|f\\(?:or_statement\\|unction_definition\\)\\|if_statement\\|methods\\|properties\\|while_statement\\)\\'") parent 4)}>
    else % !!4 %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>

        % !!8 %  <{Matched rule: ((parent-is "\\`\\(?:else\\(?:\\(?:if\\)?_clause\\)\\)\\'") parent 4)}>
    end % if true %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>


    % see previous function %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
    % !!4 %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher matlab-ts-mode--i-block-comment-end-anchor 0)}>
    for x=1:length(C) % !!4 %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
        if varargin{2}  % !!8 %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
            continue    % !!12 %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
        end   % !!8 %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>

        break % !!8 %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
              % !!14 %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher matlab-ts-mode--i-block-comment-end-anchor 0)}>

        %!!8 %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
    end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>

    switch foo()  %!!4 %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
      case 1  %!!6 %  <{Matched rule: ((node-is "\\`\\(?:\\(?:ca\\|otherwi\\)se_clause\\)\\'") parent 2)}>

        %!!8 %  <{Matched rule: ((parent-is "\\`\\(?:case_clause\\|otherwise_clause\\|switch_statement\\)\\'") parent 2)}>
      otherwise %!!6 %  <{Matched rule: ((node-is "\\`\\(?:\\(?:ca\\|otherwi\\)se_clause\\)\\'") parent 2)}>

        %!!8 %  <{Matched rule: ((parent-is "\\`\\(?:case_clause\\|otherwise_clause\\|switch_statement\\)\\'") parent 2)}>
    end %!!4 %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>

    try %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
        % !!8 %  <{Matched rule: ((parent-is "\\`\\(?:catch_clause\\|try_statement\\)\\'") parent 4)}>
    catch %!!4 %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>

        % !!8 %  <{Matched rule: ((parent-is "\\`\\(?:catch_clause\\|try_statement\\)\\'") parent 4)}>
    end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>

end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>

function B = continuations_and_block_comments %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
% !!0 %  <{Matched rule: (matlab-ts-mode--i-doc-comment-matcher matlab-ts-mode--i-doc-comment-anchor matlab-ts-mode--i-doc-comment-offset)}>
% !!0 %  <{Matched rule: (matlab-ts-mode--i-doc-comment-matcher matlab-ts-mode--i-doc-comment-anchor matlab-ts-mode--i-doc-comment-offset)}>
% !!0 %  <{Matched rule: (matlab-ts-mode--i-doc-comment-matcher matlab-ts-mode--i-doc-comment-anchor matlab-ts-mode--i-doc-comment-offset)}>

    arg1=1; %  <{Matched rule: ((parent-is "\\`function_definition\\'") parent matlab-ts-mode--set-function-indent-level-for-gp)}>


    % Block comment indicators MUST be on a line by themselves. %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
    %{ Not a block comment } %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher matlab-ts-mode--i-block-comment-end-anchor 0)}>

    foo(1); % !!4   - don't indent this special %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>

    %} Not an end to a block comment { %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>

    foo(arg1, ... %!!4 %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
        arg2);  %!!8 %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>

    foo_long_fcn(arg1, ... %!!4 %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
                 arg2); %!!17 %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>

    A = [ 1 2  % !!4 %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
          3 4 ]; % !!10 %  <{Matched rule: (matlab-ts-mode--i-row-matcher matlab-ts-mode--i-row-matcher-anchor matlab-ts-mode--i-row-matcher-offset)}>

    foo(['this is a very long string', ... %!!4 %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
         'with a continution to do something very exciting']);%!!9 %  <{Matched rule: ((parent-is "\\`\\(?:function_output\\|row\\)\\'") parent 0)}>

    set(gcf,'Position',[ 1 2 3 4], ... !!4 %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
        'Color', 'red');  % !!12 %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>

    B = A + 1 + 4 ... %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
        + 6; % !!8 %  <{Matched rule: ((parent-is "\\`binary_operator\\'") parent 0)}>

    foo_code();  % eol-comment !!4 %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
                 % continuation-comment !!17 %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher matlab-ts-mode--i-block-comment-end-anchor 0)}>

    % !!4 -blank between this & continuation comment %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
    % !!4 - more comments %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher matlab-ts-mode--i-block-comment-end-anchor 0)}>

    if condition1 || ...  % !!4 %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
       fcn_call(arg1, ... % !!12 %  <{Matched rule: ((parent-is "\\`\\(?:\\(?:boolea\\|compariso\\)n_operator\\)\\'") parent 0)}>
                arg2)  % !!21 %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
        line_in_if(); %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
    end  % !!4 %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>



end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>

function has_nested_fcn %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

    plot(1:10); %!!4 %  <{Matched rule: ((parent-is "\\`function_definition\\'") parent matlab-ts-mode--set-function-indent-level-for-gp)}>

    A = 1; %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>

    function_end_same_line(1); %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
    function_after_end_same_line(); %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>

function b=function_end_same_line(a), b=a; end %!!0 %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>

function function_after_end_same_line()%!!0 %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
%!!0 %  <{Matched rule: (matlab-ts-mode--i-doc-comment-matcher matlab-ts-mode--i-doc-comment-anchor matlab-ts-mode--i-doc-comment-offset)}>
    disp('foo');%!!4 %  <{Matched rule: ((parent-is "\\`function_definition\\'") parent matlab-ts-mode--set-function-indent-level-for-gp)}>

    debug_cmd_dual(); %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>

end%!!0 %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>

function debug_cmd_dual () %  <{Matched rule: (matlab-ts-mode--i-top-level matlab-ts-mode--column-0 0)}>
% These dbstop command dual content have 'if' blocks in them. %  <{Matched rule: (matlab-ts-mode--i-doc-comment-matcher matlab-ts-mode--i-doc-comment-anchor matlab-ts-mode--i-doc-comment-offset)}>
% The command dual detection needs to block these from being %  <{Matched rule: (matlab-ts-mode--i-doc-comment-matcher matlab-ts-mode--i-doc-comment-anchor matlab-ts-mode--i-doc-comment-offset)}>
% detected as block initiators which would cause indentaiton. %  <{Matched rule: (matlab-ts-mode--i-doc-comment-matcher matlab-ts-mode--i-doc-comment-anchor matlab-ts-mode--i-doc-comment-offset)}>

    dbstop in hRandomFile at 14 if func() % !!4 %  <{Matched rule: ((parent-is "\\`function_definition\\'") parent matlab-ts-mode--set-function-indent-level-for-gp)}>
    dbstop in hRandomFile at 30@1 if x==1 % !!4 %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
    dbstop in hPFile                      % !!4 %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
    dbstop in hSimpleFile at 2            % !!4 %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
    dbstop if error                       % !!4 %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>

    %!!4 %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>

    debug_cmd_dual(); %!!4 %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>

end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
