% BaseImage handle class - all images inherits from this class
% Package: @BaseImage
% Description: 
% Tested : Matlab R2018a
% Author : Eran O. Ofek (Mar 2021)
% Dependencies: @convert, @celestial
% Example : 
% Reliable: 2
%--------------------------------------------------------------------------

classdef MaskImage < ImageComponent
    
    properties (Dependent) % Access image data directly        
    end
    
    properties (SetAccess = public)
        Dict BitDictionary                      % The dictionary of a bit mask image        
    end
    
    methods % Constructor
       
        function Obj = MaskImage
            
            
        end

    end
 

 
    methods % Setters/Getters
     
    end
    
    methods (Static)  % static methods
       
    end
    
    methods % functionality
        function bitwise_cutout(Obj, Operator, Args)
            % Apply bitwise operator to a cutouts
            
            arguments
                Obj
                Operator function_handle
                Args.XY
                Args.Radius
                Args.Shape
            end
           
            
            
        end
        
        
        
    end
    

    methods % Unit-Test
        function Result = unitTest()
            Astro = AstroImage;
            Result = true;
        end
    end
    

end

            
