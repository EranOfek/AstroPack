% AstroImage database adaptor

classdef AstroImageDb < Component
    
    properties (Hidden, SetAccess = public)

    
    end
    

    methods % Constructor    
        
        function Obj = AstroImageDb
            
        end

    end
    
    
    methods
        function Result = insertAstroHeader(Obj, Header, Args)
            arguments
                Obj
                Header AstroHeader
                Args.Fields = {};       % As
            end
            
            
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
            io.msgStyle(LogLevel.Test, '@start', 'AstroImageDb test started')
    
            

            io.msgStyle(LogLevel.Test, '@passed', 'AstroImageDb test passed')
            Result = true;
        end
    end    
             

end

            
