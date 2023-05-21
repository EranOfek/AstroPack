
classdef FileProcessor < Component
    % Parent class for file based processing
    %
    % Poll input folder for new files:
    %   Call derived processFileImpl() function 
    %   Delete or move the processed file to archive folder
    %   Clean archive folder after specified numberof days
    
    
    % Properties
    properties (SetAccess = public)
              
        % Folders
        InputPath = ''              % Input files folder
        InputMask = '*.*'           %         
        ProcessedPath = ''          % Optional archived input files folder
        OutputPath = ''             % Optional output folder  (response/result of input files)
        
        % 
        KeepProcessedFiles = true   % true to keep the processed files in ProcessedPath, otherwise deleted after processing
        KeepOutputFiles = true      % 
        ProcessFilesMaxAge = 7      % Number of days to keep processed files in Processed Path
        OutputFilesMaxAge = 7       % Number of days to keep output files in Output path
        EnableDelete = false        % False by default to avoid accidents! USE WITH CARE
        LastCleanTime = tic();      %
        
        %
        ProcessFileFunc = []        % function (FileProcessor, FileName)

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
            %             'InputPath' - 
            %             'InputMask' - 
            %             'ProcessedPath' - 
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
        end
              
               
        function Result = process(Obj, Args)         
            % Input   : - Object
            %           - DelayMS - 
            %
            %           * Pairs of ...,key,val,...
            %             The following keys are available:            			            
            %             'DelaySec' -
            %                      
            % Output  : 
            % Author  : Chen Tishler (2021)
            % Example : 
            %           
            arguments
                Obj
                Args.DelaySec = 0.01;
            end
            
            Obj.msgLog(LogLevel.Debug, 'inputLoop: %s', Obj.InputPath);                            
            while true
                
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
                        catch
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
                        catch
                            Obj.msgLog(LogLevel.Error, 'exception trying to move file: %s', ProcessedFileName);
                            try
                                if isfile(FileName)
                                    Obj.msgLog(LogLevel.Debug, 'Deleting procesed input file: %s', FileName);                            
                                    delete(FileName);                                
                                end
                            catch
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
            catch
            end
        end
        
        
        function Result = processFileImpl(Obj, FileName)
            % Derived function, Process single input file
            Obj.msgLog(LogLevel.Verbose, 'processFileImpl - OVERWRITE this function in derived class, FileName: %s', FileName);
            Result = 0;
        end
        
        
        function Result = deleteOldFiles(Obj, Path, Mask, DeleteBefore)
            % Delete files before specified date
            % Input   : - Object
            %           - Path
            %           - Mask
            %           - DeleteBefore
            %
            % Output  : -
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
    end    
        
end

