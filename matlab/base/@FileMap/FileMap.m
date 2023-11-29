
classdef FileMap < Component
    % FileMap is a map object used to solve the problem that in deployed
    % (compiled) MATLAB applications, it is not possible to call addpath()
    % and therefore we need a mechanism to find files.
    %
    %
    % This code should exist in startup.m, to create the map file:
    %
    %       fprintf('\nSTARTUP: Calling FileMap to scan and create files map...\n');
    %       FileMap.getSingleton().addPathFolders();
    %       FileMap.getSingleton().scanFolders();
    %       FileMap.getSingleton().saveMap();
    %
    %
    % See also:
    %       base/fileMapFind.m - Global function 
    %       util/+io/+files/load2.m - Example of using fileMapFind()
    %

    % Properties
    properties (SetAccess = public)

        %
        DirList = {};           %
        Map = [];               % conainers.map
        IgnoreCase = false;     %
        StorageFileName = ''    %
        LogDuplicates = true;   %
    end

    %--------------------------------------------------------
    methods

        % Constructor
        function Obj = FileMap()
            % Constructor for FileProcessor
            % Input   : - struct array, table, cell array, matrix,
            %
            %           * Pairs of ...,key,val,...
            %             The following keys are available:
            %             'InputPath' -
            %             'InputMask' -
            %             'ProcessedPath' -
            % Output  : - New instance of FileProcessor object
            % Author  : Chen Tishler (2021)
            % Example :

            Obj.setName('FileMap');
            Obj.Map = containers.Map();
            
            % Set default filename
            if isunix
                Obj.StorageFileName = '/tmp/AstroPackFileMap_1.mat';
            else
                Obj.StorageFileName = 'c:/temp/AstroPackFileMap_1.mat';
            end

            Obj.msgLog(LogLevel.Info, 'created, StorageFileName: %s', Obj.StorageFileName);
        end
    end


    methods

        function Result = addPathFolders(Obj)
            %
            % Input   : - Path
            %
            %           * Pairs of ...,key,val,...
            %             The following keys are available:
            %
            % Output  : - Full file name
            % Author  : Chen Tishler (Dec. 2022)
            % Example :
            arguments
                Obj
            end

            Result = '';
            List = strsplit(path, ';');
            for i=1:numel(List)
                Path = List{i};
                if ~startsWith(Path, '/usr/local/MATLAB') && ~contains(Path, '\Matlab\')
                    Obj.addFolder(List{i});
                end
            end
            Obj.msgLog(LogLevel.Info, 'addPathFolders: total %d folders', numel(Obj.DirList));
        end


        function Result = addFolder(Obj, Path)
            % Add the specified path to Obj.DirList
            % Input   : - Path
            %
            %           * Pairs of ...,key,val,...
            %             The following keys are available:
            %
            % Output  : - Full file name
            % Author  : Chen Tishler (Dec. 2022)
            % Example :

            Result = '';
            if ~any(strcmp(Obj.DirList, Path))
                Obj.DirList{end+1} = Path;
            end
        end


        function Result = findFile(Obj, FileName, Args)
            % Find the specified file in the map
            % Input   : - File name
            %
            %           * Pairs of ...,key,val,...
            %             The following keys are available:
            %             'Single' - 
            %
            % Output  : - Full file name
            % Author  : Chen Tishler (Dec. 2022)
            % Example :
            arguments
                Obj
                FileName                    %
                Args.Single = true          %
            end
            
            Result = '';           
            Obj.msgLog(LogLevel.Info, 'findFile: %s', FileName);
            if ~contains(FileName, '/') && ~contains(FileName, '\')
                FName = FileName;
                if Obj.IgnoreCase
                    FName = lower(FName);
                end                
                if Obj.Map.isKey(FName)
                    F = Obj.Map(FName);
                    if Args.Single
                        if numel(F.folder) == 1
                            Result = fullfile(F.folder{1}, FileName);
                            Obj.msgLog(LogLevel.Info, 'findFile: Single - Found in map: %s', Result);
                        else
                            Obj.msgLog(LogLevel.Info, 'findFile: Single - Found multiple in map: %s', FName);
                        end
                    else
                        Result = fullfile(F.folder, FileName);
                        Obj.msgLog(LogLevel.Info, 'findFile: Not found in map: %s', FName);
                    end
                else
                    % Not found in map
                    Obj.msgLog(LogLevel.Info, 'findFile: File not found in map: %s', FName);
                end
            else
                Result = FileName;
            end
        end


        function scanFolders(Obj)
            % Update Obj.Map by calling scanPath() for all items in Obj.DirList
            % Input   : -
            %
            %           * Pairs of ...,key,val,...
            %             The following keys are available:
            %
            % Output  : - Full file name
            % Author  : Chen Tishler (Dec. 2022)
            % Example :
            Obj.msgLog(LogLevel.Info, 'scanFolders: scanning %d folders...', numel(Obj.DirList));
            for i=1:numel(Obj.DirList)
                Obj.scanFolder(Obj.DirList{i});
            end
            Obj.msgLog(LogLevel.Info, 'scanFolders: done, found %d files', Obj.Map.Count);

            % Check for duplicate files
            Obj.checkDuplicateFiles();
        end


        function scanFolder(Obj, Path)

            % Get list of files and folders in any subfolder
            Files = dir(fullfile(Path, '**/*.*'));
            for i=1:numel(Files)
                if Files(i).isdir == 0
                    fname = Files(i).name;
                    if Obj.IgnoreCase
                        fname = lower(fname);
                    end                                    
                    if ~Obj.Map.isKey(fname)
                        F = struct;
                        F.name = fname;
                        F.folder{1} = Files(i).folder;
                        Obj.Map(fname) = F;
                    else
                        % Already exists - add to end of list
                        F = Obj.Map(fname);
                        if ~ismember(F.folder, Files(i).folder) 
                            F.folder{end+1} = Files(i).folder;
                            Obj.Map(fname) = F;
                        end
                    end
                end
            end
        end


        function saveMap(Obj)
            % Save FileMap object to storage file
            Obj.msgLog(LogLevel.Info, 'saveMap: Total files: %d, File: %s', Obj.Map.Count, Obj.StorageFileName);
            if ~isempty(Obj.StorageFileName)
                M = Obj.Map;
                save(Obj.StorageFileName, 'M');
            end
        end
        

        function loadMap(Obj)
            % Load FileMap object from storage file
            Obj.msgLog(LogLevel.Info, 'loadMap: Loaindg from: %s', Obj.StorageFileName);
            if ~isempty(Obj.StorageFileName)
                load(Obj.StorageFileName);
                Obj.Map = M;
                Obj.msgLog(LogLevel.Info, 'loadMap: Loaded total %d files', Obj.Map.Count);
                Obj.checkDuplicateFiles();
            end
        end


        function checkDuplicateFiles(Obj)
            % Check for duplicate files in Obj.Map
            if Obj.LogDuplicates
                K = keys(Obj.Map);
                FileCount = 0;
                DupCount = 0;
                DupListCount = 0;
                for i=1:numel(K)
                    F = Obj.Map(K{i});
                    FileCount = FileCount + numel(F.folder);
                    if numel(F.folder) > 1
                        DupCount = DupCount + 1;
                        DupListCount = DupListCount + numel(F.folder);
                        %Obj.msgLog(LogLevel.Info, 'Multiple files found: %s', F.name);
                        for j=1:numel(F.folder)
                            %Obj.msgLog(LogLevel.Info, '  %s', fullfile(F.folder{j}, F.name));
                        end
                    end
                end
                Obj.msgLog(LogLevel.Info, 'checkDuplicateFiles: Found %d files with duplicate names, %d files out of %d total files', DupCount, DupListCount, FileCount);
            end            
        end
        
            
        function clear(Obj)
            % Clear the map
            Obj.DirList = {};
            Obj.Map = containers.Map();
        end

    end


    methods(Static)

        function Result = getSingleton()
            % Return singleton object, this is the default log file
            % to be used by current process (or workspace)
            % Input:   -
            % Output: Singleton FileMap object
            % Example: SysLogFile = LogFile.getSingleton();
            persistent PersObj
            if isempty(PersObj)
                PersObj = FileMap();
            end
            Result = PersObj;
        end

    end


    % Unit test
    methods(Static)
        Result = unitTest()
    end

end
