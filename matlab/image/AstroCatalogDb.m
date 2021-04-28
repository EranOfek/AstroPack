% AstroCatalog database adaptor

classdef AstroCatalogDb < Component
    
    properties (Hidden, SetAccess = public)

    
    end
    

    methods
    
        % Constructor
        function Obj = AstroCatalogDb           
            
        end

    end
    
    % static methods
    methods (Static)
       
    end
    
    % 
    
    % setters/getters
    methods
        
    end
    
    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
        function Result = unitTest()
            io.msgStyle(LogLevel.Test, '@start', 'AstroCatalogDb test started')
    

            io.msgStyle(LogLevel.Test, '@passed', 'AstroCatalogDb test passed')
            Result = true;
        end
    end    

end

            
