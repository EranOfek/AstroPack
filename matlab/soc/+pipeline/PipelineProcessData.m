
classdef PipelineProcessData < Component
    % Parent class for pipeline data
    
    % Properties
    properties (SetAccess = public)
        
        AstroInput AstroImage          % Input image
        AstroOutput AstroImage          % Input image

    end
    
    %-------------------------------------------------------- 
    methods
        % Constructor    
        function Obj = PipelineProcessData()
        end
     

    end

    
    % Unit test
    methods(Static)
        function Result = unitTest()
            io.msgLog(LogLevel.Test, 'Started');
            
            
            
            Result = true;
            
            io.msgLog(LogLevel.Test, 'Passed');            
        end
    end    
        
end


