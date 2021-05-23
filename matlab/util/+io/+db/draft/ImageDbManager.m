% @Chen
% Component base class
% Package: 
% Description:
%--------------------------------------------------------------------------

% Parent class for FitsDb, HdfDb
classdef ImageDbManager < Component
    % Properties
    properties (SetAccess = public)
    end
    
    %-------------------------------------------------------- 
    methods
        % Constructor    
        function Obj = ImageDbManager()
        end

    end
    
    methods(Status)
        % Get database object by filename extension
        function Db = getDbByFileName(FileName)
            [filepath, name, ext] = fileparts(FileName);
            ext = lower(ext);
            if ext == ".fits"
                Db = FitsDb;
            elseif ext == ".h5"
                Db = HdfDb;
            else
                Db = 0;
            end           
            
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

