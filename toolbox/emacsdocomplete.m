% Copyright (C) 2010-2025 Free Software Foundation, Inc.
%
% Author: Eric Ludlam <zappo@gnu.org>, John Ciolfi <john.ciolfi.32@gmail.com>
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
        v = ver('MATLAB'); %#ok<*VERMATLAB>
        verNum = str2double(v.Version);
    end

    if verNum >= 25 % R2025a and later

        % Completions TBD
        disp(['Completions-Lisp:', newline, '''(']);
        disp(')');

    else % R2024b and earlier

        if verNum < 8.4
            % Pre R2014b: partial_string
            extracmd = '';
        else
            % Post R2014b: partial_string, caret, num
            extracmd = [ ', ' num2str(length(substring)) ',0' ];
        end

        substringQuoted = strrep(substring, '''', '''''');

        command = ...
            ['matlabMCRprocess_emacs = com.mathworks.jmi.MatlabMCR;' ...
             'emacs_completions_output = matlabMCRprocess_emacs.mtFindAllTabCompletions(''' ...
             substringQuoted '''' extracmd '),' ...
             'clear(''matlabMCRprocess_emacs'',''emacs_completions_output'');'];

        % Completion engine needs to run in the base workspace to know what the variables you have
        % to work with are.
        evalin('base', command);

    end

end


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

    persistent completeSw; % if completeSw(cmd), then supports -complete
    if isempty(completeSw)
        completeSw=containers.Map();
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
            if regexp(f,'\.m$')
                fid=fopen(f, 'r');
                if fid ~= -1
                    while true
                        l = fgetl(fid);
                        if ~ischar(l), break, end
                        if regexp(l,'SUPPORTS_DASH_COMPLETE')
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
            completeCmd = regexprep(substring,'^(\w+)','$1 -complete');
            disp('emacs_completions_output =');
            evalin('base',completeCmd);
            done = true;
        end
    end

end
