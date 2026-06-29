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
    %            If empty, will assume all files have the same destination
    %            and the destination is given in the destination file names
    %            (second input argument).
    %            Default is ''.
    %          * ...,key,val,...
    %            'MkDir' - A logical indicating if to create destination
    %                   directory. Default is true.
    %            'RegExp' - A logical indicating if to attempt use regular
    %                   expressions on the source name. Default is false.
    %            'Mode' - If 'f', copies SOURCE to DESTINATION, even when
    %                   DESTINATION is read-only.
    %                   Default is [].
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
        Args.Mode           = [];
    end
    
    if Args.RegExp
        if ~ischar(SourcePath)
            error('For RegExp ture SourcePath must be a char');
        end
        Files = dir(SourcePath);
        SourceFiles = regexp({Files.name}, SourceFiles, 'match');
        Flag = ~tools.cell.isempty_cell(SourceFiles);
        SourceFiles = {Files(Flag).name};

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
    
    if isempty(DestPath)
        % assumeing all files have the same destination
        DestPath = fileparts(DestFiles);
        DestPathInFile = true;
    else
        DestPathInFile = false;
    end
    
    if (ischar(DestPath) || (isstring(DestPath) && isscalar(DestPath))) && Args.MkDir
        DirCreated = true;
        mkdir(DestPath)
    else
        DirCreated = false;
    end
    
    Nfile = numel(SourceFiles);
    Destination = cell(1, Nfile);
    for Ifile=1:1:Nfile
        if isempty(SourcePath)
            Source = SourceFiles{Ifile};
        elseif iscell(SourcePath)
            Source = fullfile(SourcePath{Ifile}, SourceFiles{Ifile});
        elseif isstring(SourcePath) && numel(SourcePath) > 1
            % genPath([]) returns a per-file string array; index it correctly
            Source = fullfile(char(SourcePath(Ifile)), SourceFiles{Ifile});
        else
            Source = fullfile(char(SourcePath), SourceFiles{Ifile});
        end
        if DestPathInFile
            Destination{Ifile} = DestFiles{Ifile};
        else
            if iscell(DestPath)
                Destination{Ifile} = fullfile(DestPath{Ifile}, DestFiles{Ifile});
            elseif isstring(DestPath) && numel(DestPath) > 1
                Destination{Ifile} = fullfile(char(DestPath(Ifile)), DestFiles{Ifile});
            else
                Destination{Ifile} = fullfile(char(DestPath), DestFiles{Ifile});
            end
        end
        % make sure diirectory exist
        if ~DirCreated
            if ~isfolder(DestPath{Ifile})
                mkdir(DestPath{Ifile});
            end
        end
        % move file
        if isempty(Args.Mode)
            movefile(Source, Destination{Ifile});
        else
            movefile(Source, Destination{Ifile}, Args.Mode);
        end
    end
    
end
