% Author: Eric Ludlam <zappo@gnu.org>, John Ciolfi <john.ciolfi.32@gmail.com>

% Copyright (C) 2010-2025 Free Software Foundation, Inc.
%
% This program is free software: you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation, either version 3 of the License, or
% (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU General Public License
% along with this program.  If not, see <http://www.gnu.org/licenses/>.

function emacsdocomplete(substring)
% EMACSDOCOMPLETE - get completions of SUBSTRING
%
% This is used by Emacs TAB in matlab-shell to provide possible completions.
%

    if UseDashComplete(substring)
        return
    end

    persistent verNum;
    if isempty(verNum)
        v      = ver('MATLAB'); %#ok<*VERMATLAB>
        verNum = str2double(v.Version);
    end

    qStr = strrep(substring, '''', '''''');
    lStr = num2str(length(substring));

    if verNum >= 25 % R2025a and later

        cmd   = ['builtin(''_programmingAidsTest'', '''', ''', qStr, ''', ', lStr, ', [])'];
        cInfo = jsondecode(evalin('base', cmd)); % use base workspace for variable completions
        cMap  = dictionary('', true);

        disp(['Completions-Lisp:', newline, '''(']);
        if isfield(cInfo, "signatures")
            sigs  = cInfo.signatures;
            cSigs = iscell(sigs);
            for i = 1 : length(sigs)
                if cSigs, args = sigs{i}.inputArguments; else, args = sigs(i).inputArguments; end
                cArgs = iscell(args);
                for j = 1 : length(args)
                    if cArgs, arg = args{j}; else, arg = args(j); end
                    if isfield(arg, 'widgetData') && isfield(arg.widgetData, 'choices')
                        DispCompletionChoices(arg.widgetData.choices);
                    end
                end
            end
        elseif isfield(cInfo, 'widgetData') && isfield(cInfo.widgetData, 'choices')
            DispCompletionChoices(cInfo.widgetData.choices);
        end
        disp(')');

    else % R2024b and earlier

        if verNum < 8.4
            extracmd = ''; % Pre R2014b: partial_string
        else
            extracmd = [', ', lStr, ', 0']; % Post R2014b: partial_string, caret, num
        end
        cmd = ['matlabMCRprocess_emacs = com.mathworks.jmi.MatlabMCR;' ...
               'emacs_completions_output = matlabMCRprocess_emacs.mtFindAllTabCompletions(''' ...
               qStr '''' extracmd '),' ...
               'clear(''matlabMCRprocess_emacs'',''emacs_completions_output'');'];
        evalin('base', cmd); % run in base to get completions on base workspace variables

    end

    function DispCompletionChoices(choices)
        nChoices = length(choices);
        cChoices = iscell(choices);
        for choiceIdx = 1 : nChoices
            if cChoices, entry = choices{choiceIdx}; else, entry = choices(choiceIdx); end
            if ~cMap.isKey(entry.completion)
                cMap(entry.completion) = true;
                if isfield(entry, 'purpose'), info = [entry.purpose, ' ']; else, info = ''; end
                desc = [info, '(' entry.matchType, ')'];
                desc = regexprep(desc, '"', '\\"');
                comp = regexprep(entry.completion, '"', '\\"');
                disp(['  ("', comp, '" . "', desc, '")']);
            end
        end
    end

end % emacsdocomplete



function done = UseDashComplete(substring)
% UseDashComplete - given SUBSTRING 'CMD ARGS', run 'CMD -complete ARGS' to get completions?
%
% For substring of form 'cmd ARGS', use 'cmd -complete ARGS' to get completions if cmd is a *.m file
% and it contains the string 'SUPPORTS_DASH_COMPLETE'.
%   1. In a comment, place the string "SUPPORTS_DASH_COMPLETE"
%   2. Handle the -complete argument which produces completion strings
%      of the form:
%          'CMD_TEXT_TO_REPLACE' --> 'REPLACEMENT_TEXT'
%              'OPTION1'
%              'OPTION2'
%              ...
% Example
%   >> cd2 -complete $d/s*
%   '$d/s*' --> '/local/USER/'
%       'sub1'
%       'sub2'
%       'sub3'
%
% See details in `matlab-shell-completion-list'.
%
% TODO: remove and use function signatures.
%       https://www.mathworks.com/help/mps/restfuljson/matlab-function-signatures-in-json.html
%

    persistent completeSw; % if completeSw(cmd), then supports -complete
    if isempty(completeSw)
        completeSw = containers.Map(); % use containers.Map instead of dictionary for old releases
    end

    done = false;

    cmd = regexp(substring, '^(\w+)\s+[^\)]', 'tokens');
    if isscalar(cmd)
        cmd=cmd{1}{1};

        if completeSw.isKey(cmd)
            supportsDashComplete = completeSw(cmd);
        else
            supportsDashComplete = false; % assume
            f = which(cmd);
            if regexp(f, '\.m$')
                fid=fopen(f, 'r');
                if fid ~= -1
                    while true
                        l = fgetl(fid);
                        if ~ischar(l), break, end
                        if regexp(l, 'SUPPORTS_DASH_COMPLETE')
                            supportsDashComplete = true;
                            break
                        end
                    end
                    fclose(fid);
                end
            end
            completeSw(cmd) = supportsDashComplete;
        end

        if supportsDashComplete
            % For /path/to/cmd.ext we have /path/to/cmd.complete which
            % signals that we can get the completions by calling
            %    CMD -complete ARGS
            completeCmd = regexprep(substring, '^(\w+)', '$1 -complete');
            disp('emacs_completions_output =');
            evalin('base', completeCmd);
            done = true;
        end
    end

end

% [EOF] toolbox/emacsdocomplete.m

% LocalWords:  ciolfi ludlam zappo gmail Rprocess Sw
