
classdef ObsTask < Component
    
    % Properties
    properties (SetAccess = public)            
        
        Conn io.db.DbConnectinn
       
    end
    
    %-------------------------------------------------------- 
    methods
        % Constructor    
        function Obj = ObsTask()
            Obj.needUuid();
            Obj.msgLog(LogLevel.Debug, 'ObsTask created: %s', Obj.Uuid);
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
