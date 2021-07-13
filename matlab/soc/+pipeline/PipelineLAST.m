

classdef PipelineLAST < PipelineComponent
    % Properties
    properties (SetAccess = public)
        
        filename        
        configPath = "";
        data
        lines
        userData
        
        inputImagePath
        inputImageExt
    end
    
    %-------------------------------------------------------- 
    methods
        % Constructor    
        function Obj = PipelineLAST()
            Obj.setName('PipelineLAST');
            Obj.needUuid();
            Obj.msgLog(LogLevel.Debug, 'created: %s', Obj.Uuid);                        
        end
     

    end

    
    % Unit test
    methods(Static)
        function Result = unitTest()
            io.msgStyle(LogLevel.Test, '@start', 'PipelineLAST test started')
            
            
            io.msgStyle(LogLevel.Test, '@passed', 'PipelineLAST test passed')                       
            Result = true;
        end
    end    
        
end


