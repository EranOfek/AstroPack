
classdef PipelineStateMachine < Component
    % Top level Pipeline Manager
    
    
    % Properties
    properties (SetAccess = public)
        
        CurState PipelineState
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

    
    methods
        
        function manage(Obj)
            
            switch Obj.CurState
                
                case PipelineState.None
                    %
                    
                    
                case PipelineState.Ready
                    %
                    
                    
                case PipelineState.ImageStart
                    %
            
                    
                case PipelineState.ImageProcess
                    %                    
                    
                case PipelineState.ImageDone
                    %
                    
                    
                case PipelineState.ImageDelay
                    %
                    
                    
                case PipelineState.FatalError
                    %
                    
                    
                otherwise
                    %
                    
            end
            
            
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

