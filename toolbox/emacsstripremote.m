% Copyright 2025 Free Software Foundation, Inc.

% This program is free software: you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation, either version 3 of the License, or
% (at your option) any later version.

% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.

% You should have received a copy of the GNU General Public License
% along with this program.  If not, see <http://www.gnu.org/licenses/>.

function out = emacsstripremote(in)
% EMACSSTRIPREMOTE - strip Emacs TRAMP remote file path prefix
%
% IN can be either a string or cell array. If it's a cell array it's assumed to
% be arguments to one of the MATLAB debugger commands.
%
% OUT will be IN updated if needed.
%
% TRAMP remote file syntax:
%   /method:host:/path/to/file
%
% Examples:
%   /ssh:user@host:~/project/file.m       => ~/project/file.m
%   /ssh:user@host:/work/project/file.m   => /work/file.m
%   /ssh:user@host:C:/work/file.m         => C:/work/file.m

    re = '^/[^:]+:[^:]+:';
    if iscell(in)
        out = in;
        if length(out) >= 2 && strcmp(out{1},'in')
            out{2} = regexprep(in{2}, '^/[^:]+:[^:]+:', '', 'once');
        end
    else
        out = regexprep(in, '^/[^:]+:[^:]+:', '', 'once');
    end
end
