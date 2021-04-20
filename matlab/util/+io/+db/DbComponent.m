% FITS Database Class
%--------------------------------------------------------------------------

classdef DbComponent < Component
    % Properties
    properties (SetAccess = public)

    end
    
    %-------------------------------------------------------- 
    methods
        % Constructor    
        function Obj = DbComponent()
            %Obj.FileName = FileName;
        end
    end
    
    
    methods
        
            
    end

    
    methods(Static)
    end
    
    
    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
        function Result = unitTest()
            io.msgLog(LogLevel.Test, "Started\n");
            
            io.msgLog(LogLevel.Test, "Passed\n");
            Result = true;
        end
    end    
        
    
end


