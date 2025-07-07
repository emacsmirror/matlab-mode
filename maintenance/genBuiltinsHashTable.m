% -*- matlab-ts -*-
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
% restoredefaultpath to fix up things.
%
% The MATLAB-only functions are documented here:
% https://www.mathworks.com/help/matlab/referencelist.html
%
% Note, we can't use the system find on *.m, *.p, *.mex* because some functions such as sin do not
% have a file on disk.
%
%

    out = ['(defvar matlab-ts-mode--builtins-ht', newline, ...
           '  #s(hash-table', newline, ...
           '     test equal', newline, ...
           '     data (', newline];

    for fcnStart = 'a' : 'z'
        completionStr = evalc(['emacsdocomplete(''', fcnStart, ''')']);
        completions = split(completionStr, newline);
        for cIdx = 1 : length(completions)
            c = completions{cIdx};
            if regexp(c, ['\((?:keyword|variable|pathItem|mlappFile|mlxFile|' ...
                          'mdlFile|slxFile|sscFile|sfxFile)\)"\)'], 'once')
                % ignore non-functions
                continue
            elseif strcmp(c, 'Completions-Lisp:') || ...
                   strcmp(c, '''(') || ...
                   strcmp(c, ')') || ...
                   isempty(c)
                % ignore lisp list specifiers
                continue
            elseif regexp(c, '\(namespace)"\)', 'once')
                % TODO - handle these
                continue
            else
                % A function.
                % We have items that are demo launchers, e.g.
                %   toolbox\simulink\simulink\keymodels\vdp.m
                %   toolbox\vdynblks\vdynsolution\vdynblksBrakingStart.m
                % We could read the m-file if it's a launcher skip it or we could ignore based on
                % path location. However, these are commands that come with MATLAB, so we'll show
                % them.
                m = regexp(c, ...
                           '^[ \t]*\(("[^"]+")[ \t]+\.[ \t]+".*\((?:mFile|pFile|mex)\)"\)$', ...
                           'tokens');
                if ~isempty(m)
                    out = [out, '           ', m{1}{1}, ' t', newline];
                else
                    error(['Unexpected c: ', c]);
                end
            end
        end
    end
    out = [out, ...
           '           ))', newline, ...
           '  "Functions that come with MATLAB, Simulink, and the add-on products.', newline, ...
           'See: MATLAB >> which FunctionName")', newline];

    writelines(out, outFile, LineEnding = '\n');
end
