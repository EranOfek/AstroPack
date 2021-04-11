% VarImage class
%   This is a subclass of ImageComponent
% Description: VarImage inherits from ImageComponent and it doesn't have
%   any special capabilities. It is intended for storing variance images.
% Author : Eran Ofek (Apr 2021)
% Dependencies: 
% Example : B = BackImage;

classdef VarImage < ImageComponent
    
    properties (Hidden, SetAccess = public)
    end
    
 
    properties (Dependent) % Access image data directly        
    end
    
    properties (SetAccess = public)
    end
    
    
    methods % Constructor
       
        function Obj = VarImage(varargin)
            % Constructor of VarImage class using the superclass
            % (ImageComponent) constructor
            
            Obj@ImageComponent(varargin{:});           
        end

    end
 
    
    methods (Static) % Unit-Test
        function Result = unitTest()
            % unitTest for VarImage class
            
            B = VarImage;
            B = VarImage(B);
            B = VarImage([2 2]);
            
            Result = true;
        end
    end
    
end

           