% -*- matlab-ts -*-
%
% Copyright 2025 Free Software Foundation, Inc.
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

function genBuiltinsHashTable(outFile)
% Generate in outFile a lisp hash-table of all functions to have font-lock-builtin-face
%
% Leverage emacsdocomplete to locate all functions in a MATLAB installation. The install
% should be the latest release with all products.
%
% Running
%   genBuiltinsHashTable('fcns.el')
% will create fcns.el that should be placed in ../matlab-ts-mode--buildins.el.
%
% When running this, the MATLAB path should not include user directories.  You can use
% restoredefaultpath to fix up things and then add the Emacs-MATLAB-Mode/toolbox direcotry to the
% path.
%
% The MATLAB-only functions are documented here:
% https://www.mathworks.com/help/matlab/referencelist.html
%
% Note, we can't use the system find on *.m, *.p, *.mex* because some functions such as sin do not
% have a file on disk.

    ht = ''; % lisp hash-table string representation
    nEntries = 0;

    for fcnStart = 'a' : 'z'
        [ht, nEntries] = getHashTableEntries(fcnStart, ht, nEntries);
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
end

function [ht, nEntries] = getHashTableEntries(fcnStart, ht, nEntries)
% Call emacsdocomplete(fcnStart) to get hash-table entries

    disp(['Capturing completions for: ', fcnStart]);
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

        % A function, class, or namespace.
        % We have items that are demo launchers, e.g.
        %   toolbox\simulink\simulink\keymodels\vdp.m
        %   toolbox\vdynblks\vdynsolution\vdynblksBrakingStart.m
        % We could read the m-file if it's a launcher skip it or we could ignore based on
        % path location. However, these are commands that come with MATLAB, so we'll treat
        % them as such.
        m = regexp(c, ...
                   ['^[ \t]*\("',...
                    '([^"]+)', ...                                  % capture fcn (no quotes)
                    '"[ \t]+\.[ \t]+"', ...
                     '(.*)', ...                                    % capture optional description
                     '\(', ...
                    '(', ['mFile|pFile|mex|function|method|', ...   % capture entryType
                          'property|enumeration|', ...
                          'class|namespace|', ...
                          'keyword|variable|pathItem|mlappFile|mlxFile|', ...
                          'mdlFile|slxFile|sscFile|sfxFile'], ')' ...
                    '\)"\)$'], ...
                   'tokens');

        % Enhancement: it would be nice to hide items that aren't documented, etc. but it's not
        % clear how to do that.

        if ~isempty(m)
            fcn = m{1}{1};
            if isequal(fcn, mfilename)
                continue; % skip ourself
            end
            desc = strtrim(m{1}{2});
            entryType = m{1}{3};
            switch entryType
              case {'mFile', 'pFile', 'mex', 'function', 'method'}
                if ~isempty(desc)
                    desc = [' ;;', desc];
                end
                ht = [ht, '           "', fcn, '" t', desc, newline];
                nEntries = nEntries + 1;
              case {'property', 'enumeration'}
                ht = [ht, '           "', fcn, '" ''', entryType, newline];
                nEntries = nEntries + 1;
              case {'class', 'namespace', ...
                    'keyword', 'variable', 'pathItem', 'mlappFile', 'mlxFile', ...
                    'mdlFile', 'slxFile', 'sscFile', 'sfxFile'}
                % 'class' and 'namespace' likely have items in them. Though variables, models,
                % etc. can hide namespaces. For example, simulink.slx is a model and we have
                % simulink namspace giving items like simulink.compiler.genapp.
                [ht, nEntries] = getHashTableEntries([fcn, '.'], ht, nEntries);
              otherwise
                error(['assert - unhandled entryType: ', entryType]);
            end
        else
            error(['Unexpected c: ', c]);
        end
    end

    disp(['nEntries = ', num2str(nEntries)]);
end
