%
%--------------------------------------------------------------------------

classdef ObsGui < Component
    
    % Properties
    properties (SetAccess = public)            
        
    end
    
    %-------------------------------------------------------- 
    methods
        % Constructor    
        function Obj = ObsGui()
            Obj.needUuid();
            Obj.msgLog(LogLevel.Debug, 'ObsGui created: %s', Obj.Uuid);
        end
        
        
        % Destructor
        function delete(Obj)
            %Obj.msgLog(LogLevel.Debug, 'ObsGui deleted: %s', Obj.Uuid);
        end        
    end
    
    
    methods %
                        
    end
    
    
    methods(Static) %
        
    end
    
    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
        Result = unitTest();
            %
    
    end    
            
end
