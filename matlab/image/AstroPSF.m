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
    % Component should contain:
    % UserData
    % Config
    
    properties (Dependent) % Access image data directly        
        Image 
        Mask 
        Back 
        Var
        Cat
    end
    
    properties (SetAccess = public)
        Header AstroHeader
        PSF AstroPSF
        CatData AstroCatalog
        WCS AstroWCS
        
        % Data
        ImageData(1,1) BaseImage
        MaskData(1,1) BaseImage
        BackData(1,1) BaseImage
        VarData(1,1) BaseImage
    end
    
    methods % Constructor
       
        function Obj = AstroPSF
            
            
        end

    end
 

 
    methods % Setters/Getters
        function Obj = set.Image(Obj, Data)
            % setter for Image - store image in ImageData property
            Obj.ImageData.Data = Data;
        end
        
        function Data = get.Image(Obj)
            % getter for Image - get image from ImageData property
            Data = Obj.ImageData.Data;
        end        
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

            
