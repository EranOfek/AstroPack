% BaseImage handle class - all images inherits from this class
% Package: @BaseImage
% Description: 
% Tested : Matlab R2018a
% Author : Eran O. Ofek (Mar 2021)
% Dependencies: @convert, @celestial
% Example : 
% Reliable: 2
%--------------------------------------------------------------------------

classdef AstroPSF < ImageComponent
    % ImageComponent contains:
    % Data
    % Scale
    % ScaleMethod
    
    
    properties (Dependent) % Access image data directly        
        PSF
        VarPSF
    end
    
    properties (SetAccess = public)
        
    end
    
    methods % Constructor
       
        function Obj = AstroPSF
            
            
        end

    end
 

 
    methods % Setters/Getters
       
    end
    
    methods (Static)  % static methods
       
    end
    
    methods % functionality
        function Result = fun_unary(Obj, OperatorOperatorArgs, OutType, DataProp, DataPropOut)
            %
           
            Nobj = numel(Obj)
            
            
        end
        
    end
    

    methods % Unit-Test
        function Result = unitTest()
            Astro = AstroImage;
            Result = true;
        end
    end
    

end

            
