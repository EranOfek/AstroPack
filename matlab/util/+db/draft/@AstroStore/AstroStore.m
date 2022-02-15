% Data Storge Manager (works along with AstroDb and ImagePath)
%

% #functions (autogen)
% AstroStore -
% copyFileToStore - Copy (or move) file to storage
% createDestFolder - Create destination folder
% getBasePath - Return storage base path, used by ImagePath
% getDataPath - Get path data folder Currently we just return our DataPath, without doing anythin with the specified Impath
% getImageFileName - Get full path to image
% getImagePath - Get full path to image
% getSingleton -
% insertFile - Insert file record to database @Todo: Need to define the table structure
% manageTimerEvent - Timer callback function
% setup - Load settings from configuration Currently we work with single data folder, in the future we may enhance the functionality to support multiple data folders (need to decide the logic)
% startTimer - Setup and start timer https://www.mathworks.com/help/matlab/ref/timer-class.html https://www.mathworks.com/help/matlab/matlab_prog/timer-callback-functions.html
% stopTimer - Stop timer
% #/functions (autogen)
%

% For future development see:
%
% https://undocumentedmatlab.com/articles/explicit-multi-threading-in-matlab-part1
% https://undocumentedmatlab.com/articles/explicit-multi-threading-in-matlab-part2

classdef AstroStore < Component
    % Singletone object
    
    properties (SetAccess = public)
        ManageTimer = [];                   % Timer to manage folders
        BasePath        = '/home/last';     %
        DataPath        = '';               % Data path (single, may be extened in the future to support multiple folder)
        Db = [];                            %db.AstroDb = [];          %
    end
    

    methods % Constructor
        
        function Obj = AstroStore
            Obj.setName('AstroStore');
            Obj.setup();
            
            %Obj.startTimer();
        end
        
        
        function Result = setup(Obj)
            % Load settings from configuration
            % Currently we work with single data folder, in the future
            % we may enhance the functionality to support multiple data
            % folders (need to decide the logic)
            Obj.msgLog(LogLevel.Debug, 'setup started');
            if tools.os.iswindows()
                Obj.DataPath = Obj.Config.Data.Pipeline.AstroStore.DataPath_Win;
                if isempty(Obj.DataPath)
                    Obj.DataPath = 'C:\\Data\\Store';
                end
            else
                Obj.DataPath = Obj.Config.Data.Pipeline.AstroStore.DataPath;
                if isempty(Obj.DataPath)
                    Obj.DataPath = '/data/store';
                end
            end
                       
            % Create folder
            if ~isempty(Obj.DataPath) && Obj.DataPath(end) == filesep
                Obj.DataPath = Obj.DataPath(1:end-1)
            end
            
            if ~isfolder(Obj.DataPath)
                Obj.msgLog(LogLevel.Info, 'Creating folder: %s', Obj.DataPath);
                mkdir(Obj.DataPath);
            end
            
            % Validate folder access
            if isfolder(Obj.DataPath)
                Obj.msgLog(LogLevel.Debug, 'folder validated: %s', Obj.DataPath);
                Result = true;
            else
                Obj.msgLog(LogLevel.Fatal, 'Failed to access data path: %s', Obj.DataPath);
                Result = false;
            end
                   
            % Create db adaptor
            Obj.Db = db.AstroDb.get();
            
            Obj.msgLog(LogLevel.Debug, 'setup done');
        end
        
        
        function Result = startTimer(Obj)
            % Setup and start timer
            % https://www.mathworks.com/help/matlab/ref/timer-class.html
            % https://www.mathworks.com/help/matlab/matlab_prog/timer-callback-functions.html
            Obj.ManageTimer = timer;
            %Obj.ManageTimer.StartDelay = 1;
            Obj.ManageTimer.Period = 1;
            Obj.ManageTimer.ExecutionMode = 'fixedRate';
            Obj.ManageTimer.TimerFcn = @Obj.manageTimerEvent;
            %Obj.ManageTimer.UserData = Obj;
            start(Obj.ManageTimer);
            Result = true;
        end

        
        function Obj = stopTimer(Obj)
            % Stop timer
            stop(Obj.ManageTimer);
        end
        
        
        function manageTimerEvent(Obj, Timer, Event)
            % Timer callback function
            disp(datestr(Event.Data.time,'dd-mmm-yyyy HH:MM:SS.FFF'));
        end

        
        function Result = getBasePath(Obj)
            % Return storage base path, used by ImagePath
            Result = Obj.BasePath;
        end
    end
    
    
    methods
        
        function [Result, Dest] = copyFileToStore(Obj, SrcFileName, DstFileName, Args)
            % Copy (or move) file to storage
            arguments
                Obj
                SrcFileName                 % Source file name
                DstFileName                 % Destination file/folder name
                Args.Move = false;          % True to move source file, otherwise it will be copied
            end
                
            %
            Result = false;
            Dest = '';
            try

                % Prepare destination file name
                if ~isempty(DstFileName) && DstFileName(end) == filesep
                    DstFileName = [Obj.DataPath, filesep, DstFileName];
                end
                
                [SrcPath, SrcName, SrcExt] = fileparts(SrcFileName);
                [DstPath, DstName, DstExt] = fileparts(DstFileName);
                         
                % Not specified
                if isempty(DstFileName)
                    DstFileName = [Obj.DataPath, filesep, SrcName, SrcExt];
                    
                % Folder name specified
                elseif isempty(DstName) && ~isempty(DstPath)
                    DstFileName = [DstPath, filesep, SrcName, SrcExt];
                    Obj.createDestFolder(DstFileName);
                    
                % Folder and name specified
                else
                    DstFileName = [Obj.DataPath, filesep, DstFileName];
                    Obj.createDestFolder(DstFileName);
                end
                
                % Move
                if Args.Move
                    Obj.msgLog(LogLevel.Info, 'moving file: %s -> %s', SrcFileName, DstFileName);
                    movefile(SrcFileName, DstFileName);
                    Result = isfile(DstFileName);
                    if Result
                        Dest = DstFileName;
                    end
                    
                % Copy
                else
                    Obj.msgLog(LogLevel.Debug, 'copying file: %s -> %s', SrcFileName, DstFileName);
                    copyfile(SrcFileName, DstFileName)
                    Result = isfile(DstFileName);
                    if Result
                        Dest = DstFileName;
                    end
                end
            catch
                % Handle error
                Obj.msgLog(LogLevel.Error, 'Exception - copying file: %s -> %s', SrcFileName, DstFileName);
            end

        end
  
        
        
        function Result = createDestFolder(Obj, DstFileName)
            % Create destination folder
            
            Result = false;
            [DstPath, DstName, DstExt] = fileparts(DstFileName);
            if ~isfolder(DstPath)
                Obj.msgLog(LogLevel.Info, 'Creating folder: %s', DstPath);
                mkdir(DstPath);
                if isfolder(DstPath)
                    Result = true;
                else
                    Obj.msgLog(LogLevel.Error, 'Failed to create folder: %s', DstPath);
                end
            end
        end
        
                    
        function Result = insertFile(Obj, SrcFileName, DstFileName, Args)
            % Insert file record to database
            % @Todo: Need to define the table structure
            arguments
                Obj
                SrcFileName         % Source file name
                DstFileName         % Destination file name
                Args.Move = false;  % ?
                Args.TableName      % Table name
                Args.ImPath         % Image path
            end

            Obj.msgLog(LogLevel.Info, 'insertFile: %s', SrcFileName);
            Count1 = Q.selectCount('master_table');
            T = tic();
            s = [];
            for i = 1:ItersCount
                s(i).recid = Component.newUuid();
            end
            Q.insertRecord('master_table', s, 'BatchSize', BathSize);
            
        end
        

        function Result = getDataPath(Obj, ImPath)
            % Get path data folder
            % Currently we just return our DataPath, without doing anythin
            % with the specified Impath
            Result = Obj.DataPath;
        end
        
        
        function Result = getImagePath(Obj, ImPath)
            % Get full path to image
            Result = '';
        end
        
        
        function Result = getImageFileName(Obj, ImPath)
            % Get full path to image
            Result = '';
        end
        
    end
             
    
    %
    methods(Static)
        function Result = getSingleton()
            persistent Obj
            
            % Create if not exist yet
            if isempty(Obj)
                Obj = db.AstroStore;
            end
            Result = Obj;
        end
        
    end
    

    %======================================================================
    
    %======================================================================
    % Performance Test
    methods(Static)
        Result = perfTest()
        
        Result = stressTest()
        
        Result = unitTest()
    end
                 
end

            
