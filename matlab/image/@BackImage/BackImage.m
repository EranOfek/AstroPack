% BackImage class
%   This is a subclass of ImageComponent
% Description: BackImage inherits from ImageComponent and it doesn't have
%   any special capabilities. It is intended for storing background images.
% Author : Eran Ofek (Apr 2021)
% Dependencies: 
% Example : B = BackImage;

classdef BackImage < ImageComponent
    
    properties (Hidden, SetAccess = public)
    end
    
 
    properties (Dependent) % Access image data directly        
    end
    
    properties (SetAccess = public)
    end
    
    
    methods % Constructor
       
        function Obj = BackImage(varargin)
            % Constructor of BackImage class using the superclass
            % (ImageComponent) constructor
            
            Obj@ImageComponent(varargin{:});           
        end

    end
 
    
    methods (Static) % Unit-Test
        Result = unitTest()
            % unitTest for BackImage class

    end
    
end

           