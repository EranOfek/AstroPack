% @Chen
% Component base class
% Package: 
% Description:
%--------------------------------------------------------------------------

% Parent class for FitsDb, HdfDb
classdef ImageDb < Component
    % Properties
    properties (SetAccess = public)
        FileName

    end
    
    %-------------------------------------------------------- 
    methods
        % Constructor    
        function Obj = ImageDb()
        end
        
        function Result = open(Obj, FileName)
            Result = false;
        end
        
    end
    
    
    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
        function Result = unitTest()     
            Result = true;
        end
    end    
        
    
end

