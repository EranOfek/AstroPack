
classdef PipelinePartial < PipelineComponent
    % Pipeline Main class, called from command line Pipeline.m 
    
    % Properties
    properties (SetAccess = public)
        Input
    end
    
    %-------------------------------------------------------- 
    methods
        % Constructor    
        function Obj = PipelinePartial()
            Obj.setName('PipelinePartial');
            Obj.needUuid();
            Obj.msgLog(LogLevel.Debug, 'created: %s', Obj.Uuid);            
        end
    end    
 
    
    methods
        function Result = run(Obj, InputFileName)
            
            % Load input file
            Obj.Input = yaml.ReadYaml(string(InputFileName).char);            
            Obj.Input.FileName = InputFileName;
            
            % Input.InputImageFile: 
            % Input.InputImageFolder:
            % Input.MaxImagesToProcess: 1
            Result = true;
        end
    end    

    
    % Unit test
    methods(Static)   
        function Result = unitTest()
            io.msgStyle(LogLevel.Test, '@start', 'PipelinePartial test started\n');
            
            Proc = FileProcessor;
            Proc.inputLoop(100);
            
            % Done
            io.msgStyle(LogLevel.Test, '@passed', 'PipelinePartial test passed')
            Result = true;            
        end
    end        
end

