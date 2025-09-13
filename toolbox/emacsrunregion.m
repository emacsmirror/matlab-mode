% Copyright (C) 2019-2025 Free Software Foundation, Inc.
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

function emacsrunregion(file, startchar, endchar)
% EMACSRUNREGION - Run code from FILE between STARTCHAR and ENDCHAR.
%
% Command sent by Emacs for run code sections and run-region functionality.

    file = emacsstripremote(file);

    if ~exist(file, 'file')
        error('You must save your region into a file accessible by MATLAB process.');
    end

    % Now figure out if shortFileName is on the path.
    [fullFilePath, shortFileName, extension] = fileparts(file);
    onpath = ~isempty(which(shortFileName));

    % If not on the path, temporarily switch to that directory so it and an files it references are
    % accessible
    if ~onpath
        oldpath = pwd;
        cd(fullFilePath);
        cleanup = onCleanup(@()cd(oldpath));
    end

    fileContents = fileread(file);

    endchar = min(endchar, length(fileContents));
    evalTxt = fileContents(startchar:endchar);
    evalin('base', evalTxt);

    % See if startchar and endchar are on the first column of a lines and if so display that. Note,
    % fileContents can contain POSIX newlines (LF) or be Windows CRFL (13, 10) line endings.
    if (startchar == 1 || fileContents(startchar-1) == newline) && ~isempty(regexp(fileContents(endchar), '[\r\n]', 'once'))
        startLineNum = length(strfind(fileContents(1:startchar), newline)) + 1;
        endLineNum = length(strfind(fileContents(1:endchar), newline));
        if fileContents(endchar) == 13 || endchar == length(fileContents)
            % Looking at CR or end-of-file
            endLineNum = endLineNum + 1;
        end

        regionInfo = sprintf('lines %d to %d', startLineNum, endLineNum);
    else
        regionInfo = sprintf('chars %d to %d', startchar, endchar);
    end

    % TODO - enable this display after updating tests to pass on Unix.
    % fprintf(1, 'emacsrunregion: finished running %s%s %s\n', ...
    %         shortFileName, extension, regionInfo);
end

% LocalWords:  STARTCHAR ENDCHAR startchar endchar LF CRFL
