
classdef PipelineManager < Component
    % Top level Pipeline Manager
    
    
    % Properties
    properties (SetAccess = public)
        
        FileName        
        Data
        Lines
        UserData
      
        InputImagePath
        ProcessedFolder
        OnputImageExt
        LogFile
        Config
    end
    
    %-------------------------------------------------------- 
    methods  
               
        % Constructor    
        function Obj = PipelineManager()
            Obj.LogFile = TLogFile("PipelineManager.log");
            Obj.Config = TConfig;
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
        
        function msglog(Obj, s)
            Obj.LogFile.log(s);
            disp(s);
        end

       
        function processImage(Obj, FileName)
            Obj.msglog("Processing image: " + FileName);
            
            
        end
        
        
        function Result = run(Obj)
            while true
                
                List = dir(fullfile(Obj.InputImagePath, "*.fits"));
                for i = 1:length(List)
                    if ~List(i).isdir
                        FileName = fullfile(List(i).folder, List(i).name);
                        Obj.msglog(FileName);
                        Obj.processImage(FileName);
                                           
                        ProcessedFileName = fullfile(Obj.ProcessedFolder, List(i).name);                        
                        Obj.msglog("Moving image to processed folder: " + ProcessedFileName);
                        movefile(FileName, ProcessedFileName, 'f');
                    end
                end                
                
                pause(1);
            end            
            
            Result = true;
        end

        
        % Read file to lines
        function Result = pollInputImage(Obj)
            
            Obj.FileName = fullfile(Obj.ConfigPath, fname);
            Obj.Data = fileread(Obj.filename);
            Obj.Lines = strsplit(Obj.data);
            Result = true;
        end
        
    end

    
    % Unit test
    methods(Static)   
        function Result = unitTest()
            fprintf("Started\n");
            Result = true;
        end
    end    
        
end

