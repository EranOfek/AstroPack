% Component base class
% Package: 
% Description:
%--------------------------------------------------------------------------

classdef Component < Base
    % Properties
    properties (SetAccess = public)
        Conf Config
        Log LogFile
    end
    
    %-------------------------------------------------------- 
    methods
        % Constructor    
        function Obj = Component()

        end
        
        function log(Obj, Msg)
            disp(Msg);
        end
    end
end

