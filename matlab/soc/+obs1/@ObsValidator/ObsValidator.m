%
%--------------------------------------------------------------------------

classdef ObsValidator < Component
    
    % Properties
    properties (SetAccess = public)            
        
    end
    
    %-------------------------------------------------------- 
    methods
        % Constructor    
        function Obj = ObsValidator()
            Obj.needUuid();
            Obj.msgLog(LogLevel.Debug, 'ObsValidator created: %s', Obj.Uuid);
        end
        
        
        % Destructor
        function delete(Obj)
            %Obj.msgLog(LogLevel.Debug, 'ObsValidator deleted: %s', Obj.Uuid);
        end        
    end
   
    
    methods
    end
    
    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
        Result = unitTest();
            %
    end
    
end
