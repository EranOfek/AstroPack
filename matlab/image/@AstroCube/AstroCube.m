% AstroCube - A container class for Cubes of images
% Properties :
%
% Functionality :
%
%



classdef AstroCube < Component
    properties (Dependent) % Access image data directly
        Var
    end
    
    properties (SetAccess = public)
        ImageData
        BackData
        VarData
        MaskData
        CCDSEC
        HeaderData AstroHeader
        WCS AstroWCS
        
        
        Nstars            = NaN;  % If Nstars=NaN, then PSF wasn't constructed yet.
    end
    
    methods % Constructor
       

    end
    
   
    methods (Static) % UnitTest
        Result = unitTest()
            % unitTest for AstroPSF
    end
    

end

           
