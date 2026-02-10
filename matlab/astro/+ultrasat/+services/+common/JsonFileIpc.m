%==========================================================================
% Project     : ULTRASAT SOC
% Filename    : ultrasat.services.common.JsonFileIpc.m
% Author      : Chen Tishler
% Created     : 02/11/2021
% Modified    : 10/02/2026
% Description : JSON file IPC class
%==========================================================================

classdef JsonFileIpc < Component
    % JsonFileIpc monitors a directory for incoming JSON files, processes them,
    % and then archives or deletes the files according to configuration.
    %
    % It supports custom processing logic via a callback or by overriding
    % processFileImpl, and includes a watchdog for monitoring the processing loop.
    %
    % Used for file-based inter-process communication and batch processing
    % pipelines (e.g., SNR <-> Python server IPC).

    % Properties
    properties (SetAccess = public)
              
        % Input parameters
        InputPath = ''              % Path of the folder where input files are located
        InputMask = '*.json'        % File mask to filter files in the input folder (default: '*.*')    
        ProcessedPath = ''          % Optional - Path of the folder to store processed files (archive folder)
        KeepProcessedFilesDays = 7  % Number of days to keep processed files in Processed Path
        Callback = []               % Function handle that points to the function for processing files
        WatchdogFileName = []       % Name of the watchdog file used to monitor the proces
        WatchdogInterval = 10       % Time interval (in seconds) for the watchdog process
        MaxRunTime = []             % Maximum runtime allowed for the JsonFileIpc instance

        % Runtime data
        StartTime = []              % Time at which the JsonFileIpc instance started        
        AlreadyProcessedFiles = []  % List of already processed input files to ignore, used if move/delete failed        
        LastCleanTime = tic();      % Timer to track the last clean-up operation in the archive folder
        LastWatchdogTime = tic();   % Timer to track the last clean-up operation in the archive folder        
    end
    
    %-------------------------------------------------------- 
    methods  
               
        % Constructor    
        function Obj = JsonFileIpc(Args)          
            % Constructor  
            arguments
                Args.InputPath = '';             % Path of the folder where input files are located
                Args.InputMask = '*.json';       % File mask to filter files in the input folder (default: '*.json')
                Args.ProcessedPath = '';         % Optional - Path of the folder to store processed files (archive folder)
                Args.KeepProcessedFilesDays = 7  % Number of days to keep processed files in Processed Path
                Args.Callback = [];              % Function handle that points to the function for processing files
                Args.WatchdogFileName = [];      % Name of the watchdog file used to monitor the proces
                Args.WatchdogInterval = 10;      % Time interval (in seconds) for the watchdog process
                Args.MaxRunTime = [];            % Maximum runtime allowed for the JsonFileIpc instance
            end
            
            Obj.setName('JsonFileIpc');            
            
            % Apply input parameters
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
            if ~isempty(Args.Callback)
                Obj.Callback = Args.Callback;
            end
            if ~isempty(Args.WatchdogFileName)
                Obj.WatchdogFileName = Args.WatchdogFileName;
            end
            if ~isempty(Args.WatchdogInterval)
                Obj.WatchdogInterval = Args.WatchdogInterval;
            end
            if ~isempty(Args.MaxRunTime)
                Obj.MaxRunTime = Args.MaxRunTime;
            end

            Obj.msgLog(LogLevel.Info, 'InputPath: %s, ProcessedPath: %s', ...
                strrep(Obj.InputPath, '\', '/'), strrep(Obj.ProcessedPath, '\', '/'));

            % Create input and processed folders
            if ~isfolder(Obj.InputPath)
                mkdir(Obj.InputPath);
            end
            if ~isfolder(Obj.ProcessedPath)
                mkdir(Obj.ProcessedPath);
            end

            % Set the start time
            Obj.StartTime = datetime('now', 'TimeZone', 'UTC');
        end
              
               
        function Result = processLoop(Obj, Args)
            % Main method to start the file processing loop.
            % This method continually checks the input folder for new files,
            % processes them, and then handles file archiving or deletion.
            arguments
                Obj
                Args.DelaySec = 0.1;        % Delay in seconds between processing cycles
                Args.MaxProcessTime = Inf;  % Default is to run indefinitely
            end
            
            Obj.msgLog(LogLevel.Debug, 'inputLoop: %s', strrep(Obj.InputPath, '\', '/'));
            startTime = tic;
            while true
                 % Check if the processing time has exceeded the maximum allowed time
                if toc(startTime) > Args.MaxProcessTime
                    break;
                end
                
                % Do one tick of the loop
                Obj.tick();
                pause(Args.DelaySec);

                % Check if the processing time has exceeded the maximum allowed time
                if Obj.shouldTerminate()
                    break
                end
            end
                
            Obj.msgLog(LogLevel.Debug, 'processLoop done: %s', Obj.InputPath);                            
            Result = true;
        end


        function tick(Obj)
            % Do one tick of the loop

            % Process a single file
            Obj.processSingleInputFile();

            % Clean old files
            Obj.cleanOldFilesFromProcessedFolder();

            % Update watchdog file
            Obj.updateWatchdogFile();
        end


        function processSingleInputFile(Obj)
            % Process a single file
                
            % Get sorted list of all files in input folder
            List = dir(fullfile(Obj.InputPath, Obj.InputMask));
            
            [~, IndexList] = sort(string({List.name}), 2, 'ascend');               
            
            % Process file by file
            for i = IndexList
                if ~List(i).isdir
                    % Process single file
                    FileName = fullfile(List(i).folder, List(i).name);

                    % Check if the file is already processed
                    if ismember(FileName, Obj.AlreadyProcessedFiles)
                        Obj.msgLog(LogLevel.Info, 'processSingleInputFile: file already processed, ignored: %s', strrep(FileName, '\', '/'));
                        continue;
                    end

                    % Do the processing
                    Obj.processJsonFile(FileName);

                    % Move file to 'processed' folder
                    if ~Obj.moveOrDeleteProcessedFile(FileName)
                        % Add to the list of already processed files
                        Obj.AlreadyProcessedFiles(end+1) = FileName;
                    end

                    % Stop after 
                    return;
                end
            end                
        end        
        
        
        function processJsonFile(Obj, FileName)
            % Process input JSON file: load, call callback, write output
            Obj.msgLog(LogLevel.Info, 'processJsonFile started: %s', strrep(FileName, '\', '/'));
            try                            
                % Read the input JSON file
                fid = fopen(FileName);
                RawBytes = fread(fid, inf);
                Text = char(RawBytes');
                fclose(fid);

                % Parse JSON from string to struct                    
                Obj.msgLog(LogLevel.Debug, 'JSON: %s', Text);
                InputStruct = jsondecode(Text);

                % Call the callback function
                try
                    OutputStruct = Obj.Callback(InputStruct);
                catch Ex
                    Obj.msgLog(LogLevel.Error, 'processJsonFile: Error calling callback function: %s', Ex.message);
                    OutputStruct = struct;
                    OutputStruct.status = 'error';
                    OutputStruct.message = sprintf('Error calling callback function: %s', Ex.message);
                end

                % Dump output struct to JSON string
                OutputText = jsonencode(OutputStruct, 'PrettyPrint', true);

                % Write to a temporary file, then rename to the final output file
                TmpFileName = strcat(FileName, '.out.tmp');
                OutputFileName = strcat(FileName, '.out');
                fid = fopen(TmpFileName, 'wt');
                fprintf(fid, OutputText);
                fclose(fid);
                movefile(TmpFileName, OutputFileName);
                
                % Log the output
                Obj.msgLog(LogLevel.Info, 'processJsonFile done: %s', strrep(OutputFileName, '\', '/'));
            catch Ex
                Obj.msgLog(LogLevel.Error, 'processJsonFile: %s: %s', FileName, Ex.message);
                return;
            end
        end
        
       
        function Result = moveOrDeleteProcessedFile(Obj, FileName)                              
            % Move or delete the processed file, return true if the file was moved or deleted
            Result = false;
            try
                % Move input file to 'processed' folder                
                if ~isempty(Obj.ProcessedPath)
                    [~, name, ext] = fileparts(FileName);
                    FName = [name, ext];                        
                    ProcessedFileName = fullfile(Obj.ProcessedPath, FName);
                    Obj.msgLog(LogLevel.Debug, 'Moving input file to processed folder: %s', strrep(ProcessedFileName, '\', '/'));                            
                    movefile(FileName, ProcessedFileName, 'f');                           
                    Result = ~isfile(FileName);

                % Otherwide delete it
                else
                    Obj.msgLog(LogLevel.Debug, 'Deleting input file: %s', strrep(FileName, '\', '/'));                                                                    
                    delete(FileName);
                    Result = ~isfile(FileName);                    
                end
            catch Ex
                Obj.msgLog(LogLevel.Error, 'moveOrDeleteProcessedFile: exception trying to move or delete file: %s', strrep(ProcessedFileName, '\', '/'));
            end            
        end


        function cleanOldFilesFromProcessedFolder(Obj)
            % Clean old processed files from the processed files folder
            Elapsed = toc(Obj.LastCleanTime);
            if Elapsed > 10
                if ~isempty(Obj.ProcessedPath) && Obj.KeepProcessedFilesDays > 0
                    Obj.deleteOldFiles(Obj.ProcessedPath, '*', now - Obj.KeepProcessedFilesDays);
                end
                Obj.LastCleanTime = tic();
            end            
        end


        function deleteOldFiles(Obj, Path, Mask, DeleteBeforeDate)
            % Scans a directory and deletes files that are older than the specified date.
            List = dir(fullfile(Path, Mask));
            for i = 1:length(List)
                if ~List(i).isdir
                    FileName = fullfile(List(i).folder, List(i).name);
                    if List(i).datenum < DeleteBeforeDate
                        Obj.msgLog(LogLevel.Debug, 'deleteOldFiles: %s', FileName);
                        delete(FileName);
                    end
                end
            end            
        end
        

        function updateWatchdogFile(Obj)
            if Obj.WatchdogInterval > 0 && ~isempty(Obj.WatchdogFileName)
                Elapsed = toc(Obj.LastWatchdogTime);
                if Elapsed >= Obj.WatchdogInterval
                    tools.os.updateWatchdogFile(Obj.WatchdogFileName, Obj.WatchdogInterval);
                    Obj.LastWatchdogTime = tic();
                end
            end
        end

        function Result = shouldTerminate(Obj)
            % Check if the JsonFileIpc instance should terminate by MaxRunTime
            Result = false;
            if Obj.MaxRunTime > 0
                CurrentTime = datetime('now', 'TimeZone', 'UTC');
                ElapsedTime = CurrentTime - Obj.StartTime;
                if ElapsedTime > Obj.MaxRunTime 
                    Obj.msgLog(LogLevel.Info, 'terminating input loop after MaxRunTime hours: %f', hours(ElapsedTime))
                    Result = true;
                end                    
            end
        end

    end

end
