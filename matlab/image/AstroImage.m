% BaseImage handle class - all images inherits from this class
% Package: @BaseImage
% Description: 
% Tested : Matlab R2018a
% Author : Eran O. Ofek (Mar 2021)
% Dependencies: @convert, @celestial
% Example : 
% Reliable: 2
%--------------------------------------------------------------------------

classdef AstroImage < Component
    % Component should contain:
    % UserData
    % Config
    
    properties (Dependent)
        % Access image data directly
        Image 
        Mask 
        Back 
        Var
        Cat
    end
    properties (SetAccess = public)
        Header Header
        PSF PSF
        CatData CatAst 
        WCS WCS
        % data
        ImageData(1,1) BaseImage
        MaskData(1,1) BaseImage
        BackData(1,1) BaseImage
        VarData(1,1) BaseImage
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
            
        end
    end
    
    

end

            
