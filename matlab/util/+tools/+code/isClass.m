function IsClass = isClass(FileName)
    % Check if an m file is a class (containing classdef statement)
    % Input  : - File name or a cell array of lines in the file.
    % Output : - A logical indicating if the file is a class.
    % Author : Eran Ofek (May 2022)
    % Example: IsClass = tools.code.isClass(FileName)

    % Read the file
    if iscell(FileName)
        SpStr = FileName;
    else
        SpStr = io.files.file2str(FileName, 'cell');
    end
    
    % check if a class
    if iscellstr(SpStr) 
        Matched = regexp(SpStr, '[^%]\s*[^'']classdef \w+', 'match');

        Matched = regexp(SpStr, '^classdef \w+', 'match');
        if sum(~cellfun(@isempty, Matched))>0
            IsClass = true;
        else
            IsClass = false;
        end
    else
        IsClass = false;
    end
    
end
