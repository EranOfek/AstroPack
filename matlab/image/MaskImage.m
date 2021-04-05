% BaseImage handle class - all images inherits from this class
% Package: @BaseImage
% Description: 
% Tested : Matlab R2018a
% Author : Eran O. Ofek (Mar 2021)
% Dependencies: @convert, @celestial
% Example : 
% Reliable: 2
%--------------------------------------------------------------------------

classdef MaskImage < Component
    
    properties (Dependent) % Access image data directly  
        Image
    end
    
    properties (SetAccess = public)
        MaskData ImageCompnent
        Dict BitDictionary                      % The dictionary of a bit mask image        
    end
    
    methods % Constructor
       
        function Obj = MaskImage
            
            
        end

    end
  
    methods % Setters/Getters
        function Result = get.Image(Obj)
            % getter for mask data
            Result = Obj.MaskData.Image;
        end
        
        function Obj = set.Image(Obj, Val)
            % setter for mask data
            if ~isinteger(Val)
                error('Mask image must be integers');
            end
            Obj.MaskData.Image = Val;
        end
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

            
