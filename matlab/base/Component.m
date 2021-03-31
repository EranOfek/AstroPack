% Component base class
% Package: 
% Description:
%--------------------------------------------------------------------------

classdef Component < Base
    % Properties
    properties (SetAccess = public)
        %Config Configuration
        %Log LogFile
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

