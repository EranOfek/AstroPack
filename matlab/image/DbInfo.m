% BaseImage handle class - all images inherits from this class
% Package: @BaseImage
% Description: 
% Tested : Matlab R2018a
% Author : Eran O. Ofek (Mar 2021)
% Dependencies: @convert, @celestial
% Example : 
% Reliable: 2
%--------------------------------------------------------------------------

classdef DbInfo < Component
    
    properties (Hidden, SetAccess = public)

        % Access image data directly
        Image 
        Mask 
        Back 
        Var 
        Header 
        PSF 
        Cat 
        WCS WCS
        % data
        ImageData(1,1) SimpleImage
        MaskData(1,1) MaskImage
        BackData(1,1) SimpleImage
        VarData(1,1) SimpleImage
        HeaderData Header
        PSFData PSF
        CatData AstCat
        
        % Images
        BImage BaseImage  % but BaseImage is even lower???
        BBack BaseImage
    end
    
    
    %-------------------
    %--- Constructor ---
    %-------------------
    methods
       
        function Obj = AstroImage
            
            
        end

    end
    
 
    
    % Setters/Getters
    methods
        function Obj = set.Image(Obj, Data)
            Obj.BImage.setData(Data); %#ok<MCSUP>
        end
        
        function Data = get.Image(Obj)
            Data = Obj.BImage.getData();
        end        
    end
    
    % static methods
    methods (Static)
       
    end
    
    % 
    
    % setters/getters
    methods
        
    end
    
    % static methods
    methods (Static)

        function Result = unitTest()
            Astro = AstroImage;
            
            Result = true;
        end
    end
    
    

end

            
