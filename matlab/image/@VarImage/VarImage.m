% VarImage class
%   This is a subclass of ImageComponent
% Description: VarImage inherits from ImageComponent and it doesn't have
%   any special capabilities. It is intended for storing variance images.
% Author : Eran Ofek (Apr 2021)
% Dependencies:
% Example : B = BackImage;

% #functions (autogen)
% VarImage - Constructor of VarImage class using the superclass (ImageComponent) constructor
% #/functions (autogen)
%

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
            for Iobj=1:1:numel(Obj)
                Obj(Iobj).DataType = AstroDataType.Var;
            end
        end

    end
 
    
    methods (Static) % Unit-Test
        Result = unitTest()
            % unitTest for VarImage class
        
    end
    
end

           
