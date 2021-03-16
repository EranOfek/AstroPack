% @Chen
% Component base class
% Package: 
% Description:
%--------------------------------------------------------------------------

% Parent class for FitsDb, HdfDb
% https://support.hdfgroup.org/HDF5/doc/H5.intro.html
classdef HdfDb < ImageDb
    % Properties
    properties (SetAccess = public)
        %config          % Configuration 
        %log             % Log file
    end
    
    %-------------------------------------------------------- 
    methods
        % Constructor    
        function Obj = HdfDb()
        end
        
        % 
    end
end

