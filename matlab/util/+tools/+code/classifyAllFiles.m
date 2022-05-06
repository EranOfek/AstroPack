function AllFiles = classifyAllFiles(Args)
    % Classify matlab-related files.
    %   This function recursively select all files under some directory.
    %   It classify the files and directory.
    %   The classification is described in the outpu.
    % Input  : * ...,key,val,...
    %            'Path' - Directory under which to select all files
    %                   recursively. If empty, use tools.os.getAstroPackPath
    %                   which return the name of the AstroPack path.
    %                   Default is [].
    %            'FileTemplate' - File name template to search.
    %                   Default is '*'.
    %            'SearchExtensions' - A cell array of file extension to classify.
    %                   Default is {'.m','.cpp','.c','.mlx'}.
    % Output : - A structure array of file names like the dir function
    %            output, and with the following additional fields:
    %            .IsPackageDir
    %            .IsClassDir
    %            .FileInPackage
    %            .FileInClass
    %            .Extension
    %            .PackNames
    %            .Report
    % Author : Eran Ofek (May 2022)
    % Example: AllFiles = tools.code.classifyAllFiles
   
    
    
    
    arguments
        Args.Path               = [];
        Args.FileTemplate       = '*';
        Args.SearchExtensions   = {'.m','.cpp','.c','.mlx'};
    end
    
    if isempty(Args.Path)
        Args.Path = tools.os.getAstroPackPath;
    end
    
    Next = numel(Args.SearchExtensions);
    
    PWD = pwd;
    cd(Args.Path);
    
    AllFiles = io.files.rdir(Args.FileTemplate);
    
    Nf = numel(AllFiles);
    [AllFiles(1:1:Nf).IsPackageDir]    = deal(false);
    [AllFiles(1:1:Nf).IsClassDir]      = deal(false);
    [AllFiles(1:1:Nf).FileInPackage]   = deal(false);
    [AllFiles(1:1:Nf).FileInClass]     = deal(false);
    [AllFiles(1:1:Nf).Extension]       = deal('');
    
    for If=1:1:Nf
        % for each file/dir
        if AllFiles(If).isdir
            % Directory - check if package/class
            switch AllFiles(If).name(1)
                case '+'
                    % a package dir
                    AllFiles(If).IsPackageDir = true;
                case '@'
                    % a class dir
                    AllFiles(If).IsClassDir   = true;
                otherwise
                    % do nothing
            end
            AllFiles(If).PackNames = tools.code.identifySubPackagesInFolder(AllFiles(If).folder);
        else
            % File
            
            % Check if file resides in a package
            if contains(AllFiles(If).folder, '+')
                AllFiles(If).FileInPackage = true;
            end
            
            % Check if file resides in a class
            if contains(AllFiles(If).folder, '@')
                AllFiles(If).FileInPackage = true;
            end
            
            % Check file extension
            switch AllFiles(If).name(1)
                case '.'
                    % a hiddem file
                    
                otherwise
                    % search for one of the request file extensions
                    [~,Name,Ext] = fileparts(AllFiles(If).name);
                    if ~isempty(Ext)
                        for Iext=1:1:Next
                            if strcmp(Ext, Args.SearchExtensions{Iext})
                                % extension found
                                AllFiles(If).Extension = Ext;
                            end
                        end
                    end
            end
            
            % Check the file content only if identified extension
            switch AllFiles(If).Extension
                case '.m'
                    AllFiles(If).Report = tools.code.analyzeMfile(fullfile(AllFiles(If).folder, AllFiles(If).name));
                otherwise
            end
            
        end
    end
    
    cd(PWD);
end


