
%--------------------------------------------------------------------------


classdef PipelineProcess < PipelineComponent
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
        function Obj = PipelineProcess()
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


