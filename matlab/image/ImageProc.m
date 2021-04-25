% 
classdef ImageProc < Component
    % Parent class for all image processing
    
    properties (Dependent)

    end
    
    properties (SetAccess = public)

    end

    
    properties (Hidden, SetAccess = public)
  
    end
    
    
    methods % Constructor
        function Obj = ImageProc()
        end
    end
    
    
    methods % getters/setters
        
    end
    
    methods % conversion
        end
        
    end
    
    
    methods (Static)  % unitTest
        function Result = unitTest
            % unitTest for ImageProc
			io.msgLog(LogLevel.Test, 'ImageProc test started');
			
			
			io.msgLog(LogLevel.Test, 'ImageProc test passed');
            
            Result = true;            
            
        end        
    end    
end
