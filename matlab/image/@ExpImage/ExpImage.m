% ExpImage class
%   This is a subclass of ImageComponent
% Description: ExpImage inherits from ImageComponent and it doesn't have
%   any special capabilities. It is intended for storing background images.
% Author : Eran Ofek (May 2023)
% Dependencies:
% Example : B = ExpImage;

% #functions (autogen)
% BackImage - Constructor of BackImage class using the superclass (ImageComponent) constructor
% #/functions (autogen)
%

classdef ExpImage < ImageComponent
    
    properties (Hidden, SetAccess = public)
    end
    
 
    properties (Dependent) % Access image data directly
    end
    
    properties (SetAccess = public)
    end
    
    
    methods % Constructor
       
        function Obj = ExpImage(varargin)
            % Constructor of BackImage class using the superclass
            % (ImageComponent) constructor
            
            Obj@ImageComponent(varargin{:});
            for Iobj=1:1:numel(Obj)
                Obj(Iobj).DataType = AstroDataType.ExpTime;
            end
            
        end

    end
 
    
    methods (Static) % Unit-Test
        Result = unitTest()
            % unitTest for BackImage class

    end
    
end

           
