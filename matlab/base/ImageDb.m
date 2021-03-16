% @Chen
% Component base class
% Package: 
% Description:
%--------------------------------------------------------------------------

% Parent class for FitsDb, HdfDb
classdef ImageDb < handle
    % Properties
    properties (SetAccess = public)
        FileName

    end
    
    %-------------------------------------------------------- 
    methods
        % Constructor    
        function Obj = ImageDb()
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

