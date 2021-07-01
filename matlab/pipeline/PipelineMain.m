
%--------------------------------------------------------------------------


classdef PipelineMain < PipelineComponent
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
        function Obj = PipelineMain()
            Obj.setName('PipelineMain');
            Obj.needUuid();
            Obj.msgLog(LogLevel.Debug, 'created: %s', Obj.Uuid);            
        end
    end

    
 
    methods
        function Result = main()
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


