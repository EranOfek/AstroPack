% BaseImage handle class - all images inherits from this class
% Package: @BaseImage
% Description: 
% Tested : Matlab R2018a
% Author : Eran O. Ofek (Mar 2021)
% Dependencies: @convert, @celestial
% Example : 
% Reliable: 2
%--------------------------------------------------------------------------

classdef BackImage < ImageComponent
    
    properties (Hidden, SetAccess = public)
    end
    
 
    properties (Dependent) % Access image data directly        
    end
    
    properties (SetAccess = public)
    end
    
    
    methods % Constructor
       
        function Obj = BackImage
            
            
        end

    end
 
    
    methods (Static)  % static methods
       
    end

    
    methods % Unit-Test
        function Result = unitTest()
            Image = BackImage;
            Result = true;
        end
    end
    
end

           