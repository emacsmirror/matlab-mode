% -*- matlab-ts -*-


% See https://github.com/acristoffers/tree-sitter-matlab/issues/94


if a
    try
        fuzzyWatchOn
    catch me
    end
    % trailing space after fuzzyWatchOff
    fuzzyWatchOff   
end
