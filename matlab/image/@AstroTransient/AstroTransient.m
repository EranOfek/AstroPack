% Class for astronomical transients data
%
%

classdef AstroTransient < Component
    
    properties
        RA
        Dec
        Table AstroCatalog              % Measurments
        Cutouts
        DiffCutouts
    end
    
    methods % constructor
       
    end
    
    methods % populate utilities
        function Obj = searchAddToTable(Obj, AI, Args)
            % Search new measurments of transient in AstroImage and add them to AstroTransient
            
        end
        
        function Obj = searchAddToCutouts(Obj, AI, Args)
            % Search new cutouts of transient in AstroImage and add them to AstroTransient
        
        end
        
    end
    
    methods % display
        function ds9(Obj, Args)
            % Open selected cutouts in ds9
            
        end
        
    end    
    
    
    methods (Static) % Unit-Test
        Result = unitTest()
    end
    
end
