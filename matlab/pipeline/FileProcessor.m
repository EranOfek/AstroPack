
classdef FileProcessor < Component
    % Parent class for processing input files
    
    
    % Properties
    properties (SetAccess = public)
              
        % Folders
        InputPath = ''              %
        ProcessedPath = ''          %
        OutputPath = ''             %
        
        % 
        KeepProcessedFiles = true   %
        KeepOutputFiles = true      %
        KeepProcessFilesAge = 7     %
        KeepOutputFilesAge = 7      %
    end
    
    %-------------------------------------------------------- 
    methods  
               
        % Constructor    
        function Obj = FileProcessor()

        end
        
        
        function init(Obj)
            Obj.Config.load('D:/Ultrasat/git/src/matlab/Pipeline/PipelineManager/pipeline.yml');
            Obj.InputImagePath = Obj.Config.Yaml.GcsInterface.InputImagePath;
            Obj.ProcessedFolder = Obj.Config.Yaml.GcsInterface.ProcessedImagePath;            
        end
        
        
        function main(Obj)
            Obj.init();
            Obj.run();
        end
        
        
        function processFile(Obj, FileName)
            % Process single input file
            Obj.msgLog(LogLevel.Verbose, 'Processing input file: ' + FileName);
            
            % Call handler in derived class
            try
                Obj.doProcessFile(FileName)
            catch
            end
            
            % Move to procesed files folder
            
            
        end
        
        
        
        function Result = processInputFolder(Obj)
            % 
            while true
                
                List = dir(fullfile(Obj.InputImagePath, "*.fits"));
                for i = 1:length(List)
                    if ~List(i).isdir
                        FileName = fullfile(List(i).folder, List(i).name);
                        Obj.msgLog(LogLevel.Verbose, FileName);
                        Obj.processFile(FileName);
                                           
                        ProcessedFileName = fullfile(Obj.ProcessedFolder, List(i).name);                        
                        Obj.msglog("Moving image to processed folder: " + ProcessedFileName);
                        movefile(FileName, ProcessedFileName, 'f');
                    end
                end                
                
                pause(DelayMS);
            end            
            
            Result = true;
        end

        
        
        function Result = inputLoop(Obj, DelayMS)
            % 
            while true
                
                List = dir(fullfile(Obj.InputImagePath, "*.fits"));
                for i = 1:length(List)
                    if ~List(i).isdir
                        FileName = fullfile(List(i).folder, List(i).name);
                        Obj.msgLog(LogLevel.Verbose, FileName);
                        Obj.processFile(FileName);
                                           
                        ProcessedFileName = fullfile(Obj.ProcessedFolder, List(i).name);                        
                        Obj.msglog("Moving image to processed folder: " + ProcessedFileName);
                        movefile(FileName, ProcessedFileName, 'f');
                    end
                end                
                
                pause(DelayMS);
            end            
            
            Result = true;
        end

        
        
        function Result = deleteOldFiles(Obj, Path, Mask, DeleteBefore)
            % Delete files before specified date
            List = dir(fullfile(Path, Mask));
            for i = 1:length(List)
                if ~List(i).isdir
                    FileName = fullfile(List(i).folder, List(i).name);
                    if List(i).datenum < DeleteBefore
                        Obj.msgLog(LogLevel.Verbose, 'deleteOldFiles: %s', FileName);
                        delete(FileName)
                    end
                end
            end            
            Result = true;
        end
        
        
        % Read file to lines
        function Result = pollInput(Obj)
            
            Obj.FileName = fullfile(Obj.ConfigPath, fname);
            Obj.Data = fileread(Obj.filename);
            Obj.Lines = strsplit(Obj.data);
            Result = true;
        end
        
    end

    
    % Unit test
    methods(Static)   
        function Result = unitTest()
            io.msgStyle(LogLevel.Test, '@start', 'FileProcessor test started\n');
            
            
            % Done
            io.msgStyle(LogLevel.Test, '@passed', 'FileProcessor test passed')
            Result = true;            
        end
    end    
        
end

