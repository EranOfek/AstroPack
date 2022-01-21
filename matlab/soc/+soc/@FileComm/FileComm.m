
classdef FileComm < Component
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
        ProcessedPath = ''          % Optional archived input files folder
        OutputPath = ''             % Optional output folder  (response/result of input files)
        InputFileMask = '*.*'       % 
        
        % 
        KeepProcessedFiles = true   % true to keep the processed files in ProcessedPath, otherwise deleted after processing
        KeepOutputFiles = true      % 
        ProcessFilesMaxAge = 7      % Number of days to keep processed files in Processed Path
        OutputFilesMaxAge = 7       % Number of days to keep output files in Output path
        EnableDelete = false        % False by default to avoid accidents! USE WITH CARE
                        
        %
        ProcessFileFunc = []        % function (FileProcessor, FileName)

    end
    
    %-------------------------------------------------------- 
    methods  
               
        % Constructor    
        function Obj = FileProcessor()
            Obj.setName('FileProcessor');
        end
        
        
        function init(Obj, Args)
            % Initialize with default settings
            
            arguments
                Obj
                Args.Conf = []
            end
            
            if ~isempty(Args.Conf)
                Obj.InputImagePath = Conf.InputPath;
                Obj.ProcessedFolder = fullfile(Obj.InputImagePath, 'processed');
            end
            
            % Create folder
            if ~isfolder(Obj.ProcessedFolder)
                mkdir(Obj.ProcessedFolder);
            end
            
            ReadExt = false;
            if ReadExt && ~isempty(Args.Conf)
                Obj.ProcessedFolder = Conf.ProcessedPath;
                Obj.OutputPath = Conf.OutputPath;
                Obj.ProcessFilesMaxAge = Conf.ProcessFilesMaxAge;
                Obj.OutputFilesMaxAge = Conf.OutputFilesMaxAge;
            end
            
            %Obj.Config.load('D:/Ultrasat/git/src/matlab/Pipeline/PipelineManager/pipeline.yml');
            %Obj.InputImagePath = Obj.Config.Yaml.GcsInterface.InputImagePath;
            %Obj.ProcessedFolder = Obj.Config.Yaml.GcsInterface.ProcessedImagePath;            
        end
              
               
        function Result = inputLoop(Obj, DelayMS, Args)         
            % Poll input folder with specified delay, perform single step
            % in DelayMS == -1
            arguments
                Obj
                DelayMS
                Args
            end
            
            Obj.msglog(LogLevel.Debug, "inputLoop: " + Obj.InputImagePath);                            
            while true
                
                % Get sorted list of all files in input folder
                List = dir(fullfile(Obj.InputImagePath, Obj.InputFileMask));
                
                [~, IndexList] = sort(string({List.name}), 2, 'ascend');               
                
                % Process file by file
                for i = IndexList
                    if ~List(i).isdir
                        
                        % Process single file
                        FileName = fullfile(List(i).folder, List(i).name);
                        Obj.msgLog(LogLevel.Verbose, FileName);
                        
                        % This calls the derived function processFileImpl()
                        Obj.processFile(FileName);
                                           
                        % Move file to 
                        if ~isempty(Obj.ProcessedFolder)
                            ProcessedFileName = fullfile(Obj.ProcessedFolder, List(i).name);                        
                            Obj.msglog(LogLevel.Debug, "Moving input file to processed folder: " + ProcessedFileName);                            
                            movefile(FileName, ProcessedFileName, 'f');
                            
                        else
                            % @Todo: What should we do if there is no
                            % "processed files" folder? Should we delete
                            % the processed files?
                            if Obj.DeleteProcessed
                                Obj.msglog(LogLevel.Debug, "Deleting procesed input file: " + FileName);                            
                                delete(FileName);
                            end
                        end
                    end
                end                
                
                % Stop after single file
                if DelayMS < 0
                    break;
                end                
                pause(DelayMS);
            end            
            
            % Clean old files
            % @Todo: Move to timer function, no need to do it here
            if ~isempty(Obj.ProcessedPath) && Obj.ProcessFilesMaxAge > 0
                Obj.deleteOldFiles(Obj.ProcessedPath, '*', now - Obj.ProcessFilesMaxAge);
            end
                
            Obj.msglog(LogLevel.Debug, "inputLoop done: " + Obj.InputImagePath);                            
            Result = true;
        end
        
        
        function processFile(Obj, FileName)
            % Process single input file, this is just a wrapper around the
            % derived processFileImpl()
            Obj.msgLog(LogLevel.Verbose, 'Processing input file: ' + FileName);
            
            % Call handler in derived class
            try
                        
                % Call user callback function (event)
                if ~isempty(Obj.ProcessFileFunc)
                    Obj.ProcessFileFunc(Obj, FileName);
                else
                    Obj.processFileImpl(FileName)
                end
            catch
            end
        end
        
        
        function Result = processFileImpl(Obj, FileName)
            % Derived function, Process single input file
            Obj.msgLog(LogLevel.Verbose, 'processFileImpl: ' + FileName);
            Result = 0;
        end
        
        
        function Result = deleteOldFiles(Obj, Path, Mask, DeleteBefore)
            % Delete files before specified date
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
        Result = unitTest();
            % Unit-Test

    end    
        
end

