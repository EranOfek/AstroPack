% SciImage class
%   This is a subclass of ImageComponent
% Description: SciImage inherits from ImageComponent and it doesn't have
%   any special capabilities. It is intended for storing science images.
% Author : Eran Ofek (Apr 2021)
% Dependencies:
% Example : B = SciImage;

% #functions (autogen)
% SciImage - Constructor of SciImage class using the superclass (ImageComponent) constructor
% #/functions (autogen)
%

classdef SciImage < ImageComponent
    
    properties (Hidden, SetAccess = public)
    end
    
 
    properties (Dependent) % Access image data directly
    end
    
    properties (SetAccess = public)
        IsBackSubtracted(1,1) logical          = false;
    end
    
    properties
        % Time-tag / X-ray related properties
        %Photons(1,1) PhotonsList
    end
    
    
    methods % Constructor
       
        function Obj = SciImage(varargin)
            % Constructor of SciImage class using the superclass
            % (ImageComponent) constructor
            
            Obj@ImageComponent(varargin{:});
            for Iobj=1:1:numel(Obj)
                Obj(Iobj).DataType = AstroDataType.Data;
            end
        end

    end
    
    
    methods (Static) % Unit-Test
        Result = unitTest()
            % unitTest for BackImage class

    end
    
end

           
