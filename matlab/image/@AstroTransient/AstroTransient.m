% Class for astronomical transients data
%
%

classdef AstroTransient < Component
    
    properties
        RA
        Dec
        Table AstroCatalog              % Measurments
        Cutouts
        
    end
    
    methods % constructor
       
    end
    
    
    methods (Static) % Unit-Test
        Result = unitTest()
    end
    
end
