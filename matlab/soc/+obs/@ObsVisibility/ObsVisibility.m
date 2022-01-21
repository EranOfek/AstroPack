%
%--------------------------------------------------------------------------

classdef ObsVisibility < Component
    
    % Properties
    properties (SetAccess = public)            
    end
    
    %-------------------------------------------------------- 
    methods
        % Constructor    
        function Obj = ObsVisibility()
            Obj.needUuid();
            Obj.msgLog(LogLevel.Debug, 'ObsVisibility created: %s', Obj.Uuid);
        end        
        
        % Destructor
        function delete(Obj)
            %
        end        
    end
    
    
    methods %
    end
    
    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
        Result = unitTest();
            %
    end    
            
end
