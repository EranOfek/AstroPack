function Out = removeChars(Str, CharToRemove)
    % Remove all specific chars (default is blank) from char, cell, string array.
    % Input  : - A char array, a cell array of char arrays or a string
    %            array.
    %          - Character to remove. Default is ' '.
    % Output : - The input with the removed chars.
    % Author : ChatGPT + Eran Ofek (Apr 2026)
    % Example: tools.string.removeChars("a    a a")
    
    arguments
        Str
        CharToRemove = ' '
    end

    if isstring(Str)
        Out = erase(Str, CharToRemove);
    elseif iscell(Str)
        Out = cellfun(@(S) localRemove(S, CharToRemove), Str, 'UniformOutput', false);
    elseif ischar(Str)
        Out = localRemove(Str, CharToRemove);
    else
        error('Input must be char, string, or cell array.');
    end
end

function Out = localRemove(S, CharToRemove)
    if isstring(S)
        Out = erase(S, CharToRemove);
    elseif ischar(S)
        Out = S(~ismember(S, CharToRemove));
    else
        error('Elements must be char or string.');
    end
end
