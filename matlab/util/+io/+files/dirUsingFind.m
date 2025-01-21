function [FileName, S] = dirUsingFind(FilePat, Args)
    % Fast, recursive, dir-like command implementing linux find function.
    %   For large recursive directories this may be an order of magnitude
    %   faster compared with io.files.rdir
    % Input  : - File pattern to search
    %          * ...,key,val,... 
    %            'CaseSens' - Use case sensetive search.
    %                   Default is false.
    %            'Path' - The path below to recursivly search for files.
    %                   If empty, then use current pwd.
    %                   Default is [].
    %            'Type' - type argument for the find command.
    %                   Default is 'f' (search for files).
    %            'OutIsStruct' - Output is structure (true), or string
    %                   array (false). Default is true.
    %            'SeperateFolder' - Seperate folder name from file name.
    %                   Default is true.
    %            'Extra' - Extra arguments that can be supplied to the find
    %                   command.
    %                   For example: '-size +1M' for files larger than 1MB
    %                   '-mtime -7' - for files modified in the past 7 days
    %
    % Output : - A string array of file names (if OutIsStruct is false),
    %            or a struct array with the following fields (per file):
    %            .name - file name.
    %            .folder - folder name (if SeperateFolder is true).
    % Author : Eran Ofek (2025 Jan) 
    % Example: R=io.files.dirUsingFind('*.m');

    arguments
        FilePat
        
        Args.CaseSens       = false;
        Args.Path           = [];
        Args.Type           = 'f';
        Args.OutIsStruct    = true;
        Args.SeperateFolder = true; 
        Args.Extra          = ''; % '-size +1M', '-mtime -7'
    end

    if Args.CaseSens
        SearchStr = '-name';
    else
        SearchStr = '-iname';
    end
    
    
    if isempty(Args.Path)
        Args.Path = pwd;
    end
    
    
    %Cmd = sprintf('find %s -type %s %s "%s"',Args.Path, Args.Type, SearchStr, FilePat);
    Cmd = sprintf('find %s -type %s %s "%s" %s 2>/dev/null',Args.Path, Args.Type, SearchStr, FilePat, Args.Extra);
    %[A,B]=system('find . -name "*.m"');
    [S, Out] = system(Cmd);
    FileName   = string(splitlines(Out));
    if numel(FileName)==1
        FileName = [];
    else
        FileName = FileName(1:end-1);
    end
    
    if Args.SeperateFolder
        % seperate file name from folder
        [Folder, FileName, Ext] = fileparts(FileName);
        FileName = join([FileName, Ext], '', 2);
    end
    
    if Args.OutIsStruct
        %N = numel(Result);
        if Args.SeperateFolder
            FileName = struct('name',{FileName{:}}, 'folder',{Folder{:}});
        else
            FileName = struct('name',{FileName{:}});
        end
        
            
    end
    
end
