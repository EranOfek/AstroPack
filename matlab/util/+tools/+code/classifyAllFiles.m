function AllFiles = classifyAllFiles(Args)
    %
    % AllFiles = tools.code.classifyAllFiles
   
    
    
    
    arguments
        Args.Path               = [];
        Args.FileTemplate       = '*';
        
        Args.SearchExtensions   = {'m','cpp','mlx'};
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
                    for Iext=1:1:Next
                        if strcmp(Ext, Args.SearchExtensions{Iext})
                            % extension found
                            AllFiles(If).Extension = Ext;
                        end
                    end
            end
            
            % Check the file content
            
            
        end
    end
    
    cd(PWD);
end


