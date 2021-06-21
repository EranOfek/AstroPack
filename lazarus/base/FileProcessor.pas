
// Parent class for file based processing
// Polll input folder for new files:
// Call derived processFileImpl() function
// Delete or move the processed file to archive folder
// Clean archive folder after specified numberof days

FileProcessor = class(TComponent)
public

        // Folders
        InputPath: String; = ''              % Input files folder
        ProcessedPath: String; = ''          % Optional archived input files folder
        OutputPath: String; = ''             % Optional output folder  (response/result of input files)
        InputFileMask: String; = '*.*'       %
        
        //
        KeepProcessedFiles: Boolean; = true   % true to keep the processed files in ProcessedPath, otherwise deleted after processing
        KeepOutputFiles: Boolean; = true      % 
        ProcessFilesMaxAge: Integer; = 7      % Number of days to keep processed files in Processed Path
        OutputFilesMaxAge: Integer; = 7       % Number of days to keep output files in Output path
    end
    
    %-------------------------------------------------------- 
    methods  
               
        constructor Create;
        function Obj = FileProcessor()
            Obj.Name = 'FileProcessor';
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
              
        
       
        function Result = inputLoop(Obj, DelayMS)
            % Poll input folder with specified delay, perform single step
            % in DelayMS == -1
            
            Obj.msglog(LogLevel.Debug, "inputLoop: " + Obj.InputImagePath);                            
            while true
                
                % Get sorted list of all files in input folder
                List = dir(fullfile(Obj.InputImagePath, Obj.InputFileMask));
                List = sort(List);
                
                for i = 1:length(List)
                    if ~List(i).isdir
                        FileName = fullfile(List(i).folder, List(i).name);
                        Obj.msgLog(LogLevel.Verbose, FileName);
                        Obj.processFile(FileName);
                                           
                        if ~isempty(Obj.ProcessedFolder)
                            ProcessedFileName = fullfile(Obj.ProcessedFolder, List(i).name);                        
                            Obj.msglog(LogLevel.Debug, "Moving input file to processed folder: " + ProcessedFileName);                            
                            movefile(FileName, ProcessedFileName, 'f');
                        end
                    end
                end                
                
                
                if DelayMS < 0
                    break;
                end
                
                pause(DelayMS);
            end            
            
            % Clean old files
            if ~isempty(Obj.ProcessedPath) && Obj.ProcessFilesMaxAge > 0
                Obj.deleteOldFiles(Obj.ProcessedPath, '*', now - Obj.ProcessFilesMaxAge);
            end
                
            Obj.msglog(LogLevel.Debug, "inputLoop done: " + Obj.InputImagePath);                            
            Result = true;
        end
        
        
        function processFile(Obj, FileName)
            % Process single input file
            Obj.msgLog(LogLevel.Verbose, 'Processing input file: ' + FileName);
            
            % Call handler in derived class
            try
                Obj.processFileImpl(FileName)
            catch
            end
            
            % Move to procesed files folder
            
            
        end
        
        
        function Result = processFileImpl(Obj, FileName)
            % Derived function, Process single input file
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
                        delete(FileName)
                    end
                end
            end            
            Result = true;
        end
        
    end

    
    % Unit test
    methods(Static)   
        function Result = unitTest()
            io.msgStyle(LogLevel.Test, '@start', 'FileProcessor test started\n');
            
            Proc = FileProcessor;
            Proc.inputLoop(100);
            
            % Done
            io.msgStyle(LogLevel.Test, '@passed', 'FileProcessor test passed')
            Result = true;            
        end
    end    
        
end


