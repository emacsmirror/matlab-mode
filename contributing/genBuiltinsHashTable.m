% -*- matlab-ts -*-
%
% Copyright (C) 2025 Free Software Foundation, Inc.
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

function genBuiltinsHashTable
% Generate a lisp hash-table file containing of all functions to have font-lock-builtin-face
%
% Leverage emacsdocomplete to locate all functions in a MATLAB installation. The install should be
% the latest release with all products.
%
% 1. Create /tmp/builtins-ht.el
%
%      matlab -nodesktop
%      >> run /path/to/Emacs-MATLAB-Mode/contributing/genBuiltinsHashTable.m
%      <snip>
%      Created: /tmp/builtins-ht.el
%      >> quit
%
%    This will restore the MATLAB path to the default path and then add the path to
%    emacsdocomplete. This is so we don't capture non-builtin items. Thus, after running, you should
%    quit MATLAB.
%
% 2. Place the contents of /tmp/builtins-ht.el in ../matlab-ts-mode--builtins.el
%    Sort the entries of the hash table using sort line case insensitive
%    (defun sort-lines-i () (interactive)(let ((sort-fold-case t))(call-interactively 'sort-lines)))
% 
%
% 3. rm /tmp/builtins-he.el
%
% Other implementation options:
% - The MATLAB-only functions are documented here in
%   https://www.mathworks.com/help/matlab/referencelist.html, but this is very incomplete so we
%   can't use it.
% - We can't use the system find on *.m, *.p, *.mex* because some functions such as sin do not have
%   a file on disk.

    restoredefaultpath
    emacsToolbox = [fileparts(fileparts(mfilename('fullpath'))), filesep, 'toolbox'];
    addpath(emacsToolbox);

    outFile = [tempdir, 'builtins-ht.el'];
    logFile = [tempdir, 'builtins-ht.log'];
    logFID = fopen(logFile, 'w');
    if logFID == -1
        error("Failed to create %s", logFile);
    end

    fprintf(1, "ProgressLog: %s\n", logFile);

    ht = ''; % lisp hash-table string representation
    nEntries = 0;

    bMap = dictionary('', true); % map of builtins we've found

    for fcnStart = 'a' : 'z'
        [ht, nEntries, bMap] = getHashTableEntries(fcnStart, ht, nEntries, bMap, logFID, 0);
    end

    ht = ['(defvar matlab-ts-mode--builtins-ht', newline, ...
          '  #s(hash-table', newline, ...
          '     test equal', newline, ...
          '     size ', num2str(nEntries), newline, ...
          '     data (', newline, ...
          ht, ...
          '           ))', newline, ...
          '  "Items that come with MATLAB, Simulink, and the add-on products.', newline, ...
          'See: MATLAB', newline, ...
          '   >> which FunctionName', newline, ...
          'or,', newline, ...
          '   >> which ClassPath  % where ClassPath is all but the last ', ...
          '\".<property|enumeration>\"")', newline];

    writelines(ht, outFile, LineEnding = '\n');
    fprintf(1, "Created: %s\n", outFile);
    fclose(logFID);
end

function [ht, nEntries, bMap] = getHashTableEntries(fcnStart, ht, nEntries, bMap, logFID, frame)
% Call emacsdocomplete(fcnStart) to get hash-table entries

    if bMap.isKey(fcnStart)
        % Running emacsdocomplete('break.a') returns entries:  aa2int, ...
        % which we've already processed. It should return nothing.
        % TODO - investigate this to see if there's a fix
        return; % skip already captured items
    end
    bMap(fcnStart) = true;

    frame = frame + 1;
    fprintf(logFID, '[%d] Capturing completions for: %s\n', frame, fcnStart);

    completionStr = evalc(['emacsdocomplete(''', fcnStart, ''')']);
    completions   = split(completionStr, newline);

    for cIdx = 1 : length(completions)
        c = completions{cIdx};
        if strcmp(c, 'Completions-Lisp:') || ...
           strcmp(c, '''(') || ...
           strcmp(c, ')') || ...
           isempty(c)
            % Ignore lisp list specifiers
            continue
        end

        % c is of format:
        %    ("<THING>" . "<OPTIONAL_DESCRIPTION>(<ENTRY_TYPE>)")
        % where THING is a function, class, namespace, etc.

        % We have items that are demo launchers, e.g.
        %   toolbox\simulink\simulink\keymodels\vdp.m
        %   toolbox\vdynblks\vdynsolution\vdynblksBrakingStart.m
        % We could read the m-file if it's a launcher skip it or we could ignore based on
        % path location. However, these are commands that come with MATLAB, so we'll treat
        % them as such.
        m = regexp(c, ...
                   ['^[ \t]*\("', ...
                    '([^"]+)', ...                                 % capture THING (no quotes)
                    '"[ \t]+\.[ \t]+"', ...
                    '(.*)', ...                                    % capture OPTIONAL_DESCRIPTION
                    '\(', ...
                    '(', ['mFile|pFile|mex|function|method|', ...  % capture ENTRY_TYPE
                          'property|enumeration|', ...
                          'class|namespace|', ...
                          'keyword|variable|pathItem|mlappFile|mlxFile|', ...
                          'mdlFile|slxFile|sscFile|sfxFile'], ')' ...
                    '\)"\)$'], ...
                   'tokens');

        if ~isempty(m)
            thing = m{1}{1};
            if isequal(thing, mfilename)
                continue; % skip ourself
            end

            if bMap.isKey(thing)
                % Running emacsdocomplete('break.a') returns entries:  aa2int, ...
                % which we've already processed. It should return nothing.
                % TODO - investigate this to see if there's a fix
                continue; % skip already captured items
            end

            entryType = m{1}{3};

            switch entryType
              case 'keyword'
                % we have these handled as keywords in matlab-ts-mode
              case {'mFile', 'pFile', 'mex', 'function', 'method', ...
                    'class', ...
                    'variable', 'pathItem', 'mlappFile', 'mlxFile', ...
                    'mdlFile', 'slxFile', 'sscFile', 'sfxFile'}
                comment = [' ;; (', entryType, ')'];
                desc = strtrim(m{1}{2});
                if ~isempty(desc)
                    comment = [comment ' ', desc]; %#ok<AGROW>
                end
                entry = ['           "', thing, '" t', comment];
                fprintf(logFID, '[%d] entry: %s\n', frame, entry);
                ht = [ht, entry, newline]; %#ok<AGROW>
                bMap(thing) = true;
                nEntries = nEntries + 1;
                if strcmp(entryType, 'mFile')
                    % Consider mFile = matlab (toolbox/matlab/general/matlab.m)
                    % We also have matlab.<namespace>, so also look for that.
                    entryType = 'namespace';
                end
            end

            switch entryType
              case {'pFile', 'mex', 'function', 'method'}
                % handled above
              case {'namespace'}
                % Ignore items under foo.bar.internal namespaces. These can appear/disappear and
                % should only be used by code provided by MathWorks.
                if isempty(regexp(thing, '\.internal$', 'once'))
                    for fcnStart = 'a' : 'z'
                        [ht, nEntries, bMap] = getHashTableEntries([thing, '.', fcnStart], ht, ...
                                                                   nEntries, bMap, logFID, frame);
                    end
                end
              case {'property', 'enumeration'}
                entry = ['           "', thing, '" ''', entryType];
                fprintf(logFID, '[%d] p/e-entry: %s\n', frame, entry);
                ht = [ht, entry, newline]; %#ok<AGROW>
                bMap(thing) = true;
                nEntries = nEntries + 1;
              case {'class', ...
                    'keyword', 'variable', 'pathItem', 'mlappFile', 'mlxFile', ...
                    'mdlFile', 'slxFile', 'sscFile', 'sfxFile'}
                % 'class' likely have items in them. Though variables, models,
                % etc. can hide namespaces. For example, simulink.slx is a model and we have
                % simulink namespace giving items like simulink.compiler.genapp.
                for fcnStart = 'a' : 'z'
                    [ht, nEntries, bMap] = getHashTableEntries([thing, '.', fcnStart], ht, ...
                                                               nEntries, bMap, logFID, frame);
                end
              otherwise
                error(['assert - unhandled entryType: ', entryType]);
            end
        else
            error(['Unexpected c: ', c]);
        end
    end

    fprintf(logFID, '[%d] nEntries = %d\n', frame, nEntries);
end

% LocalWords:  emacsdocomplete builtins keymodels vdynblks vdynsolution mlapp mlx ssc sfx genapp tmp
% LocalWords:  nodesktop defun aa
