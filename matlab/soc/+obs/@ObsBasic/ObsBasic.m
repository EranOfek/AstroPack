
classdef ObsBasic < Component
    
    % Properties
    properties (SetAccess = public)            
        
        Conn io.db.DbConnectinn
       
    end
    
    %-------------------------------------------------------- 
    methods
        % Constructor    
        function Obj = ObsBasic()
            Obj.needUuid();
            Obj.msgLog(LogLevel.Debug, 'ObsBasic created: %s', Obj.Uuid);
        end
        
        
        % Destructor
        function delete(Obj)
            %
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
