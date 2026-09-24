function [Result] = findDirBySubString(Substring, Path, AbsPath)
    % Find all directories that contain a substring (linux only).
    % Input  : - Substring defined as a string in between two "/"...
    %            If you like a substring of this, use e.g., 'string*'.
    %          - Path under which to search.
    %            If empty, use current dir. Default is [].
    %          - A logical indicating if to return absolute path (true),
    %            or relative path (false). Default is true.
    % Output : - A string array of directories containing the sub string.
    % Author : Eran Ofek (2026 Mar) 
    % Example: R=io.files.findDirBySubString('raw');

    arguments
        Substring
        Path         = [];
        AbsPath      = true;
    end

    if ~isempty(Path)
        PWD = pwd;
        cd(Path);
    end

    if AbsPath
        Str = sprintf("find . -type d -iname '%s' -exec realpath {} \\;", Substring);
    else
        % relative path
        Str = sprintf("find . -type d -iname '%s';", Substring);
    end

    [~,Result] = system(Str);
    Result = regexp(Result,'\n','split');
    Result = string(Result(1:end-1)).';


    if ~isempty(Path)
        cd(PWD);
    end


end
