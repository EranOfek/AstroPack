% AstroAngle class
%   A container for angles.
%
% Authors: Eran Ofek (Mar 2022)
%
% Functionality:
%


classdef AstroAngle < Base
    % Base class for all objects

    % Properties
    properties 
        Angle
        Units     = 'deg';   % 'deg' | 'rad'
        Range     = 'ra';    % 'ra' | 'dec' | 'ra180'
    end
    
    properties (Dependent)
        DMS
        Str
    end

    %--------------------------------------------------------
    methods
        function Obj = AstroAngle()
            % Constructor
            
            
        end
    end

    
    methods 
        
    end
    

    %----------------------------------------------------------------------
    methods(Static) % Unit test
        Result = unitTest()
            % unitTest for Base class
    end

end
