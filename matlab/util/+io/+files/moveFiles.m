function Destination = moveFiles(SourceFiles, DestFiles, SourcePath, DestPath, Args)
    % Move a list of files with options.
    %   Can be use to move files, move a list of of files with common path,
    %   create destination directories, and move file using regular
    %   expressions.
    % Input  : - Source files:
    %            This can be a single file name (char array), a cell array
    %            of file names, or a regular expression.
    %            If regular expression, set 'RegExp' to true.
    %          - Destination file names. A single file name, or a cell array
    %            of file names. If empty, then use the source name as
    %            destination.
    %            Default is [].
    %          - Source path, either a char or a cell array of paths.
    %            Default is ''.
    %          - Destination path, either a char or a cell array of paths.
    %            Default is ''.
    %          * ...,key,val,...
    %            'MkDir' - A logical indicating if to create destination
    %                   directory. Default is true.
    %            'RegExp' - A logical indicating if to attempt use regular
    %                   expressions on the source name. Default is false.
    % Output : - A cell array of destination file names including full
    %            path.
    % Author : Eran Ofek (Apr 2022)
    % Example: % move list of files from local dir to some dir.
    %          Destination = io.files.moveFiles({'a',v'}, [], '', '~/')
    %          % Move files selected by some pattern
    %          Destination = io.files.moveFiles('A*.\.txt', [], '', '~/','RegExp',true)
    
    arguments
        SourceFiles
        DestFiles           = [];
        SourcePath          = '';
        DestPath            = '';
        Args.MkDir logical  = true;
        Args.RegExp logical = false;
    end
    
    if Args.RegExp
        if ~ischar(SourcePath)
            error('For RegExp ture SourcePath must be a char');
        end
        Files = dir(SourcePath);
        SourceFiles = regexp({Files.name}, SourceFiles, 'match');
    end
    
    if isempty(DestFiles)
        DestFiles = SourceFiles;
    end
    
    if ischar(SourceFiles)
        SourceFiles = {SourceFiles};
    end
    if ischar(DestFiles)
        DestFiles = {DestFiles};
    end   
    
    if ischar(DestPath) && Args.MkDir
        mkdir(DestPath)
    end
    
    Nfile = numel(SourceFiles);
    Destination = cell(1, Nfile);
    for Ifile=1:1:Nfile
        Source      = sprintf('%s%s%s', SourcePath, filesep, SourceFiles{Ifile});
        Destination{Ifile} = sprintf('%s%s%s', DestPath, filesep, DestFiles{Ifile});
        % make sure diirectory exist
        if ~ischar(DestPath) && Args.MkDir
            mkdir(DestPath{Ifile});
        end
        % move file
        movefile(Source, Destination{Ifile});
    end
    
end
