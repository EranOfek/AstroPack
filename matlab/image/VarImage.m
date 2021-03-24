% BaseImage handle class - all images inherits from this class
% Package: @BaseImage
% Description: 
% Tested : Matlab R2018a
% Author : Eran O. Ofek (Mar 2021)
% Dependencies: @convert, @celestial
% Example : 
% Reliable: 2
%--------------------------------------------------------------------------

classdef VarImage < ImageComponent
    % Component should contain:
    % UserData
    % Config
    
    properties (Dependent) % Access image data directly        
    end
    
    properties (SetAccess = public)
    end
    
    methods % Constructor
       
        function Obj = VarImage            
        end
    end
 
    methods % Unit-Test
        function Result = unitTest()
            Image = VarImage
            Result = true;
        end
    end
   
end
            
