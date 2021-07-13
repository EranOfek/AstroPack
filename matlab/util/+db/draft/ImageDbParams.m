% @Chen
% Component base class
% Package: 
% Description:
%--------------------------------------------------------------------------

% Parent class for FitsDb, HdfDb
classdef ImageDbParams
    % Properties
    properties (SetAccess = public)
        
        %
        MinX
        MaxX
        MinY
        MaxY
        CenterX
        CenterY
        Width
        Height

    end
    
    %-------------------------------------------------------- 
    methods
        % Constructor    
        function Obj = ImageDbParams()
        end
        
        % 
    end
    
    
    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
        function Result = unitTest()     
            Result = true;
        end
    end    
        
    
end

