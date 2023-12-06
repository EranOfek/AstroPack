
classdef FileProcessor < Component
    % FileProcessor is file handling class designed for automated processing of files
    % in a specified directory. 
    %
    % The primary functionality of FileProcessor includes monitoring an input directory for new files,
    % processing these files according to user-defined criteria or functions, and then managing the 
    % post-processing lifecycle of these files - either archiving them in a separate directory or 
    % deleting them based on configuration. 
    % The class supports custom processing logic through a callback function or by extending 
    % the class and overriding the processFileImpl method. Additionally, the class
    % includes a watchdog mechanism to monitor the processing activity and ensure system stability.
    % 
    % This class is ideal for scenarios where files need to be regularly processed, such as 
    % data import/export, automated data transformation, or batch processing in a data pipeline. 
    % Its modular nature and extensible design make it well-suited for integration into larger 
    % systems or as a standalone file processing solution.
    %
    % Key Features:
    %   - Polling an input directory for new files.
    %   - Configurable file processing through callback functions or method overriding.
    %   - File management post-processing, including archiving and auto-deletion.
    %   - Cleanup mechanism to maintain the archive directory.
    %   - Watchdog functionality for process monitoring and management.
    %   - Customizable settings for file retention and processing intervals.
    %
    % Example:
    %   processor = FileProcessor('InputPath', '/path/to/input', 'ProcessedPath', '/path/to/archive');
    %   processor.process(); % Start the file processing loop
    %
    % Author: Chen Tishler (2021)
    %   
    % FileProcessor is currently used by the SNR deployed app to receive requests from the 
    % Python REST Server process, and can be used for inter-process communication by files, 
    % pipeline, etc.
    %
    
    % Properties
    properties (SetAccess = public)
              
        % Folders
        InputPath = ''              % Path of the folder where input files are located
        InputMask = '*.*'           % File mask to filter files in the input folder (default: '*.*')    
        ProcessedPath = ''          % Optional - Path of the folder to store processed files (archive folder)
        OutputPath = ''             % Optional - Path of the folder for storing output files generated from the input files
        
        % 
        KeepProcessedFiles = true   % true to keep the processed files in ProcessedPath, otherwise deleted after processing
        KeepOutputFiles = true      % true to determine if output files should be retained
        ProcessFilesMaxAge = 7      % Number of days to keep processed files in Processed Path
        OutputFilesMaxAge = 7       % Number of days to keep output files in Output path
        EnableDelete = false        % False by default to avoid accidents! USE WITH CARE
        LastCleanTime = tic();      % Timer to track the last clean-up operation in the archive folder
        
        %
        ProcessFileFunc = []        % Function handle that points to the function for processing files (FileProcessor, FileName)

        WatchdogFileName = []       % Name of the watchdog file used to monitor the proces
        WatchdogInterval = 10       % Time interval (in seconds) for the watchdog process
        StartTime = []              % Time at which the FileProcessor instance started
        MaxRunTime = []             % Maximum runtime allowed for the FileProcessor instance
    end
    
    %-------------------------------------------------------- 
    methods  
               
        % Constructor    
        function Obj = FileProcessor(Args)          
            % Constructor for FileProcessor
            % Input   : - struct array, table, cell array, matrix,
            %
            %           * Pairs of ...,key,val,...
            %             The following keys are available:            			            
            %             'InputPath' - Path to the input file directory.
            %             'InputMask' - File mask for filtering files in the input directory.
            %             'ProcessedPath' - Path to the directory where processed files are stored.
            % Output  : - New instance of FileProcessor object
            % Author  : Chen Tishler (2021)
            % Example : 

            arguments
                Args.InputPath = '';         %
                Args.InputMask = '';         %
                Args.ProcessedPath = '';     %
            end
            
            Obj.setName('FileProcessor');            
            
            if ~isempty(Args.InputPath)
                Obj.InputPath = Args.InputPath;
            end

            if ~isempty(Args.InputMask)
                Obj.InputMask = Args.InputMask;
            end
            
            if ~isempty(Args.ProcessedPath)
                Obj.ProcessedPath = Args.ProcessedPath;
            else
                Obj.ProcessedPath = fullfile(Obj.InputPath, 'processed');
            end

            % Create input folder
            if ~isfolder(Obj.InputPath)
                mkdir(Obj.InputPath);
            end
            
            % Create processed folder
            if ~isfolder(Obj.ProcessedPath)
                mkdir(Obj.ProcessedPath);
            end
            
            ReadExt = false;
            if ReadExt && ~isempty(Args.Conf)
                Obj.ProcessedFolder = Conf.ProcessedPath;
                Obj.OutputPath = Conf.OutputPath;
                Obj.ProcessFilesMaxAge = Conf.ProcessFilesMaxAge;
                Obj.OutputFilesMaxAge = Conf.OutputFilesMaxAge;
            end

            Obj.StartTime = datetime('now', 'TimeZone', 'UTC');
        end
              
               
        function Result = process(Obj, Args)
            % Main method to start the file processing loop.
            % This method continually checks the input folder for new files,
            % processes them, and then handles file archiving or deletion.
            %
            % Input   : - Object
            %           - DelayMS (float): Delay in seconds between processing cycles.
            %
            %           * Pairs of ...,key,val,...
            %             The following keys are available:            			            
            %             - 'DelaySec' - Delay in seconds between processing cycles
            %             - 'MaxProcessTime' - Specify the maximum processing time in seconds. 
            %               If not provided, the loop runs indefinitely.
            %                      
            % Output  : True if the process completes successfully.
            % Author  : Chen Tishler (2021)
            % Example : 
            %           
            arguments
                Obj
                Args.DelaySec = 0.01;       % Delay in seconds between processing cycles
                Args.MaxProcessTime = Inf;  % Default is to run indefinitely
            end
            
            Obj.msgLog(LogLevel.Debug, 'inputLoop: %s', Obj.InputPath);                            
            startTime = tic;
            while true
                 % Check if the processing time has exceeded the maximum allowed time
                if toc(startTime) > Args.MaxProcessTime
                    %Obj.msgLog(LogLevel.Info, 'MaxProcessTime reached, terminating the loop.');
                    break;
                end
                
                % Get sorted list of all files in input folder
                List = dir(fullfile(Obj.InputPath, Obj.InputMask));
                
                [~, IndexList] = sort(string({List.name}), 2, 'ascend');               
                
                % Process file by file
                for i = IndexList
                    if ~List(i).isdir
                        FName = List(i).name;
                        
                        % Skip files start with '~' (meaning that it was already processed)
                        if startsWith(FName, '~') 
                            continue
                        end
                            
                        % Process single file
                        FileName = fullfile(List(i).folder, FName);
                        Obj.msgLog(LogLevel.Verbose, 'process: %s', FileName);
                        
                        % This calls the derived function processFileImpl()
                        try
                            Obj.processFile(FileName);
                        catch Ex
                            Obj.msgLog(LogLevel.Error, 'exception in processFile: %s', ProcessedFileName);
                        end
                                           
                        % Move file to 'processed' folder
                        try
                            if ~isempty(Obj.ProcessedPath)
                                if isfile(FileName)
                                    ProcessedFileName = fullfile(Obj.ProcessedPath, FName);
                                    Obj.msgLog(LogLevel.Debug, 'Moving input file to processed folder: %s', ProcessedFileName);                            
                                    movefile(FileName, ProcessedFileName, 'f');                           
                                end
                            else
                                % Delete or rename to '~'...
                                if Obj.DeleteProcessed
                                    if isfile(FileName)
                                        Obj.msgLog(LogLevel.Debug, 'Deleting procesed input file: %s', FileName);                                                                    
                                        delete(FileName);
                                    end
                                else
                                    ProcessedFileName = fullfile(Obj.ProcessedPath, '~', FName);
                                    if isfile(FileName)
                                        movefile(FileName, ProcessedFileName, 'f');
                                    end
                                end
                            end
                        catch Ex
                            Obj.msgLog(LogLevel.Error, 'exception trying to move file: %s', ProcessedFileName);
                            try
                                if isfile(FileName)
                                    Obj.msgLog(LogLevel.Debug, 'Deleting procesed input file: %s', FileName);                            
                                    delete(FileName);                                
                                end
                            catch Ex
                                Obj.msgLog(LogLevel.Error, 'Failed to delete file: %s', FileName);                            
                            end
                        end
                    end
                end                
                
                % Clean old processed files
                Elapsed = toc(Obj.LastCleanTime);
                if Elapsed > 10
                    if ~isempty(Obj.ProcessedPath) && Obj.ProcessFilesMaxAge > 0
                        Obj.deleteOldFiles(Obj.ProcessedPath, '*', now - Obj.ProcessFilesMaxAge);
                    end
                    Obj.LastCleanTime = tic();
                end
                
                % Stop after single file
                if Args.DelaySec <= 0
                    break;
                end                
                pause(Args.DelaySec);

                % Update watchdog
                if ~isempty(Obj.WatchdogFileName)
                    tools.os.updateWatchdogFile(Obj.WatchdogFileName, Obj.WatchdogInterval);
                end

                if Obj.MaxRunTime > 0
                    CurrentTime = datetime('now', 'TimeZone', 'UTC');
                    ElapsedTime = CurrentTime - Obj.StartTime;
                    if ElapsedTime > Obj.MaxRunTime 
                        Obj.msgLog(LogLevel.Info, 'terminating input loop after MaxRunTime hours: %f', hours(ElapsedTime))
                        break
                    end                    
                end
            end
                
            Obj.msgLog(LogLevel.Debug, 'inputLoop done: %s', Obj.InputPath);                            
            Result = true;
        end
        
        
        function processFile(Obj, FileName)
            % Process single input file, this is just a wrapper around the
            % derived processFileImpl() or callback ProcessFileFunc
            Obj.msgLog(LogLevel.Verbose, 'Processing input file: %s', FileName);
            
            % Call handler in derived class
            try
                        
                % Call user callback function (event)
                if ~isempty(Obj.ProcessFileFunc)
                    Obj.ProcessFileFunc(FileName);
                    
                % Or call processFileImpl() of inherited class
                else
                    Obj.processFileImpl(FileName)
                end
            catch Ex
            end
        end
        
        
        function Result = processFileImpl(Obj, FileName)
            % Abstract method to be overridden in derived classes for file processing.
            % This method should contain the logic for processing a single file.
            %
            % FileName (string): Path of the file to be processed.
            %
            % Returns:
            %   Result (various): Result of the processing. Type depends on implementation.

            Obj.msgLog(LogLevel.Verbose, 'processFileImpl - OVERWRITE this function in derived class, FileName: %s', FileName);
            Result = 0;
        end
        
        
        function Result = deleteOldFiles(Obj, Path, Mask, DeleteBefore)
            % Scans a directory and deletes files that are older than the specified date.
            %
            % Input   : - Object
            %           - Path - Directory path to scan for old files.
            %           - Mask - File mask to filter files in the directory.
            %           - DeleteBefore (datetime): Date-time threshold. Files older than this will be deleted.
            %
            % Output  : True if the deletion process completes successfully.
            % Author  : Chen Tishler (2021)
            % Example : deleteOldFiles('/tmp/*.tmp', now-1);
            %
            List = dir(fullfile(Path, Mask));
            for i = 1:length(List)
                if ~List(i).isdir
                    FileName = fullfile(List(i).folder, List(i).name);
                    if List(i).datenum < DeleteBefore
                        Obj.msgLog(LogLevel.Debug, 'deleteOldFiles: %s', FileName);
                        if Obj.EnableDelete
                            delete(FileName);
                        else
                            Obj.msgLog(LogLevel.Debug, 'not deleted: %s', FileName);
                        end
                    end
                end
            end            
            Result = true;
        end
        
    end

    
    % Unit test
    methods(Static)   
        Result = unitTest()
            % Static method for unit testing of the FileProcessor class.
            % This method should contain various test cases to validate the functionality of the class.
            %
            % Returns:
            %   Result (boolean): True if all tests pass successfully.
    end    
        
end

