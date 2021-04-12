% BaseImage handle class - all images inherits from this class
% Package: @BaseImage
% Description: 
% Tested : Matlab R2018a
% Author : Eran O. Ofek (Mar 2021)
% Dependencies: @convert, @celestial
% Example : 
% Reliable: 2
%--------------------------------------------------------------------------

classdef MaskImage < ImageComponent    % ImageComponent & BitDictionary
    
    properties (Dependent) % Access image data directly  
%         Image
%         Data
%         Scale 
%         ScaleMethod 
%         FileName      
    end
    properties (SetAccess = public)
        Dict BitDictionary                      % The dictionary of a bit mask image        
    end
    properties (Hidden, SetAccess = public)
        % MaskData ImageComponent
    end
    
    
    methods % Constructor
       
%         function Obj = MaskImage(FileNames)
%             %
%             
%             arguments
%                 FileNames       = [];
%             end
%             
%             if isempty(FileNames)
%                 Obj.MaskData = ImageComponent([]);
%             end
%             
%         end

    end
  
    methods % Setters/Getters
%         function Result = get.Image(Obj)
%             % getter for Image (from MaskData.Image)
%             Result = Obj.MaskData.Image;
%         end
%                 
%         function Obj = set.Image(Obj, Val)
%             % setter for Image (to MaskData.Image)
%             if ~isinteger(Val)
%                 error('Mask image must be integers');
%             end
%             Obj.MaskData.Image = Val;
%         end
%         
%         function Result = get.Data(Obj)
%             % getter for Image (from MaskData.Data)
%             Result = Obj.MaskData.Data;
%         end
%         
%         function Obj = set.Data(Obj, Val)
%             % setter for Image (to MaskData.Data)
%             if ~isinteger(Val)
%                 error('Mask image must be integers');
%             end
%             Obj.MaskData.Data = Val;
%         end   
%         
%         function Result = get.Scale(Obj)
%             % getter for Scale (from MaskData.Scale)
%             Result = Obj.MaskData.Scale;
%         end
%                 
%         function Obj = set.Scale(Obj, Val)
%             % setter for Scale (to MaskData.Scale)
%             
%             Obj.MaskData.Scale = Val;
%         end
        
    end
    
    methods (Static)  % static methods
       
    end
    
    methods % functionality
        function Result = bitStat(Obj)
            %
            
            
        end
        
        function [Flag, XY, Ind] = findBit(Obj, BitNames)
            %
            
            
            
        end
        
        
%         function bitwise_cutout(Obj, Operator, Args)
%             % Apply bitwise operator to a cutouts
%             
%             arguments
%                 Obj
%                 Operator function_handle
%                 Args.XY
%                 Args.Radius
%                 Args.Shape
%             end
%            
%         end
        
    end
    
    methods % bit statistics
        function [BitDec, BitNames] = bitNames(Obj, XY)
            % 
            
        end
        
        function Result = bitStat(Obj)
            % Return the bit mask statistics
            
        end
        
    end
    

    methods % Unit-Test
        function Result = unitTest()
            Astro = AstroImage;
            Result = true;
        end
    end
    

end

            
