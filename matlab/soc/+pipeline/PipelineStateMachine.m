
classdef PipelineStateMachine < Component
    % Top level Pipeline Manager
    
    
    % Properties
    properties (SetAccess = public)
        
        CurState
    end
    
    %-------------------------------------------------------- 
    methods  
               
        % Constructor    
        function Obj = PipelineStateMachine()

        end
        
        function init(Obj)
            Obj.Config.load('D:/Ultrasat/git/src/matlab/Pipeline/PipelineManager/pipeline.yml');
            Obj.InputImagePath = Obj.Config.Yaml.GcsInterface.InputImagePath;
            Obj.ProcessedFolder = Obj.Config.Yaml.GcsInterface.ProcessedImagePath;            
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

